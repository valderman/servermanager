{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}
-- | Monad for monitoring and talking to instances.
module ServerManager.Instance.Monad
  ( MonadInst (..), Inst, InstanceException (..), InstanceInfo (..)
  , Triggers (..)
  , runInst
  , ServerManager.Instance.Monad.try, liftSh, ask, asks, sendCommand
  , logItem
  ) where
import Control.Exception hiding (try)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Shell hiding (lift, ask)
import Control.Shell.Concurrent
import Database.Selda hiding (select)
import Database.Selda.Backend
import Database.Selda.SQLite hiding (select)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Text (pack)
import Data.Time
import System.IO (hWaitForInput)

import ServerManager.Instance.Command
import ServerManager.Log
import ServerManager.ServerConfig
import ServerManager.Tables
import ServerManager.Instance.Manager.Type
import ServerManager.Instance.Supervisor
import Control.Monad.Triggered

-- | A computation over an instance.
newtype Inst cfg resp a = Inst
  { unInst :: ReaderT (InstanceInfo cfg resp) Shell a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader (InstanceInfo cfg resp)
  )

class (Typeable cfg, MonadIO m) => MonadInst cfg resp m | m -> cfg resp where
  liftInst :: Inst cfg resp a -> m a

instance Typeable cfg => MonadInst cfg resp (Inst cfg resp) where
  liftInst m = m

instance MonadInst cfg (LogMessage cfg) m =>
         MonadInst cfg (LogMessage cfg) (Triggered (LogMessage cfg) m) where
  liftInst = lift . liftInst

data InstanceInfo cfg resp = InstanceInfo
  { -- | ID of the running instance.
    infoId :: ID (Instance cfg)

    -- | Server manager root directory.
  , infoRootPath :: FilePath

    -- | Database connection to use.
  , infoConnection :: SeldaConnection

    -- | Database model for the instance.
  , infoModel :: Model cfg

    -- | Function to update the list of running instances.
  , infoModifyInstances :: InstanceUpdate Inst InstanceInfo cfg resp

    -- | Function to post a command to the current instance.
  , infoPostCommand :: Command -> Inst cfg resp ()

    -- | Parse a single line of output from the server into a log message.
    --   Filled in by the user.
  , infoParser :: ByteString -> Maybe (LogMessage cfg)

    -- | Executed after the instance is stopped. The given @Bool@ will be @True@
    --   if the shutdown was graceful, otherwise @False@.
    --   On abnormal shutdown, this trigger is only executed once all attempts
    --   to restart the instance have failed.
    --   Filled in by the user.
  , infoDeathHandler :: Bool -> Inst cfg resp ()

    -- | Writes out a server-readable list of banned players,
    --   given the root directory of the server and a list of banned players.
  , infoWriteBanList :: FilePath -> [Text] -> IO ()

    -- | Writes out a server-readable player whitelist,
    --   given the root directory of the server and a player whitelist.
  , infoWriteWhiteList :: FilePath -> [Text] -> IO ()

    -- | Writes out a server-readable ops,
    --   given the root directory of the server and a player whitelist.
  , infoWriteOpList :: FilePath -> [Text] -> IO ()

    -- | Triggers for various events.
    --   Filled in by the user.
  , infoTriggers :: Triggers cfg resp
  }

type ServerTrigger cfg resp = Trigger (LogMessage cfg) (Inst cfg resp) ()

data Triggers cfg resp = Triggers
  { -- | Trigger to be executed as the server starts.
    --   Should wait for the server to confirm that it's up and running,
    --   and then set the server state to @Running@.
    startTrigger :: ServerTrigger cfg resp

    -- | Trigger to be executed as the server commences orderly shutdown.
    --   Should tell the server to stop, and then wait for it to actually
    --   stop.
  , stopTrigger :: ServerTrigger cfg resp

    -- | Trigger to be executed when the server restarts after an abnormal
    --   shutdown. Should perform the same functionality as 'startTrigger'.
  , restartTrigger :: ServerTrigger cfg resp

    -- | Trigger to perform a backup. Should prepare the running instance
    --   for backup (i.e. pause any writing to disk), call the given computation
    --   which performs the actual backup of all instance files, and finally
    --   ensure get the server running normally (i.e. resume writing to disk)
    --   again post-backup.
    --
    --   The given computation returns @True@ if the backup succeeded, and
    --   @False@ if it failed.
  , backupTrigger :: Inst cfg resp Bool -> ServerTrigger cfg resp

    -- | Send a message to the whole instance.
  , broadcastTrigger :: Text -> ServerTrigger cfg resp

    -- | Ban the given player, with a potential expiry time and date.
    --   Ban expiry will be handled by the server manager's scheduler,
    --   so the trigger does not nexessarily need to deal with this.
  , banTrigger :: Text -> Maybe UTCTime -> ServerTrigger cfg resp

    -- | Unban the given player.
  , unbanTrigger :: Text -> ServerTrigger cfg resp

    -- | Add the given player to the server's whitelist, with a potential
    --   expiry date.
    --   Like with 'banTrigger', the whitelist expiry will be handled by the
    --   server manager's scheduler.
  , whitelistTrigger :: Text -> Maybe UTCTime -> ServerTrigger cfg resp

    -- | Remove the given player from the whiteliset.
  , unwhitelistTrigger :: Text -> ServerTrigger cfg resp

    -- | Set the operator flag of the given player.
  , opTrigger :: Text -> Bool -> ServerTrigger cfg resp

    -- | Trigger to be executed for each logged message,
    --   except those logged by other triggers.
  , logTrigger :: Trigger (LogMessage cfg) (Inst cfg resp) ()
  }

-- | Send a command to the instance supervisor, if it's running.
sendCommand :: Command -> Inst cfg resp ()
sendCommand msg = do
  postCommand <- asks infoPostCommand
  postCommand msg

newtype InstanceException = InstanceException String
  deriving Show
instance Exception InstanceException

-- | Try to execute an instance computation, returning @Left@ if an exception
--   is thrown.
try :: Inst cfg resp a -> Inst cfg resp (Either String a)
try (Inst m) = Inst $ ask >>= lift . Control.Shell.try . runReaderT m

instance MonadSelda (Inst cfg resp) where
  seldaConnection = infoConnection <$> ask
  wrapTransaction commit rollback m = do
    result <- ServerManager.Instance.Monad.try m
    case result of
      Left e  -> rollback >> throw (InstanceException e)
      Right x -> commit >> return x

-- | Run a computation over an instance.
runInst :: ServerConfig cfg
        => InstanceInfo cfg resp
        -> Inst cfg resp a
        -> Shell a
runInst info (Inst m) = do
    result <- Control.Shell.try $ runReaderT m info
    case result of
      Left e -> liftIO $ do
        logIO mdl c i $ item Severe Manager (failMessage e)
        fail e
      Right x -> do
        return x
  where
    failMessage e = "Unexpected instance failure: " <> pack (show e)
    c = infoConnection info
    i = infoId info
    mdl = infoModel info

-- | Log an item.
logItem :: MonadInst cfg resp m => LogMessage cfg -> m ()
logItem m = liftInst $ do
  info <- ask
  liftIO $ logIO (infoModel info) (infoConnection info) (infoId info) m

-- | Lift a 'Shell' computation into the 'Inst' monad.
liftSh :: Shell a -> Inst cfg resp a
liftSh = Inst . lift
