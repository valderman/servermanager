{-# LANGUAGE ScopedTypeVariables #-}
-- | Functionality to manager currently running server instances.
module ServerManager.Instance.Manager
  ( Manager, mgrConfig, mgrConnection
  , Command (..)
  , createManager
  , startInstance, stopInstance, withInstance, withInstance_
  , getState, sendCommand
  ) where
import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Shell (shell_)
import qualified Data.IntMap as M
import Data.Proxy
import Database.Selda (ID, fromId)
import Database.Selda.Backend (SeldaConnection)
import ServerManager.Tables (Instance, model)
import ServerManager.Log.Format (LogMessage)
import ServerManager.Instance.Command
import ServerManager.Instance.Monad (Inst, InstanceInfo (..), runInst)
import ServerManager.Instance.Manager.Type
import ServerManager.Instance.Supervisor
import ServerManager.Instance.Supervisor.Piped
import ServerManager.ServerConfig (ServerConfig)
import ServerManager.ServiceConfig

-- | A manager for running server instances.
type Manager cfg resp =
  InstanceManager InstanceInfo ServiceConfig cfg resp

-- | Create a new instance manager.
createManager :: MonadIO m
              => ServiceConfig cfg resp
              -> SeldaConnection
              -> m (Manager cfg resp)
createManager cfg conn = liftIO $ do
  InstanceManager <$> newMVar M.empty <*> pure cfg <*> pure conn

-- | Start the given instance, unless it's already running.
--   Returns the current state of the instance.
startInstance :: forall m cfg resp.
                 (MonadIO m, ServerConfig cfg)
              => Manager cfg resp
              -> ID (Instance cfg)
              -> m State
startInstance mgr@(InstanceManager ref cfg conn) iid = do
  liftIO $ modifyMVarMasked ref $ \m -> do
    case M.lookup key m of
      Just is -> do
        pure (m, instanceState is)
      _ -> do
        let ii = InstanceInfo
                 { infoId = iid
                 , infoRootPath = cfgRootDirectory cfg
                 , infoConnection = conn
                 , infoModel = model (Proxy :: Proxy cfg)
                 , infoModifyInstances = modifyRunningInstances mgr
                 , infoPostCommand = void . sendCommand mgr iid
                 , infoParser = cfgLogParser cfg
                 , infoDeathHandler = cfgDeathHandler cfg
                 , infoWriteBanList = cfgWriteBanList cfg
                 , infoWriteWhiteList = cfgWriteWhiteList cfg
                 , infoWriteOpList = cfgWriteOpList cfg
                 , infoTriggers = cfgTriggers cfg
                 }
        ih <- shell_ $ runInst ii start
        let is = InstanceState
              { instanceState = Starting
              , instanceHandle = ih
              , instanceInfo = ii
              }
        pure (M.insert key is m, Starting)
  where
    key = fromId iid

-- | Stop the given instance, if it's running.
--   Returns the current state of the instance.
stopInstance :: MonadIO m
             => Manager cfg resp
             -> ID (Instance cfg)
             -> m State
stopInstance (InstanceManager ref _ _) iid = liftIO $ do
  withMVarMasked ref $ \m -> do
    case M.lookup (fromId iid) m of
      Just is -> do
        stop (instanceHandle is)
        pure Stopping
      Nothing -> do
        pure Stopped

-- | Get the state of the given instance.
getState :: MonadIO m
         => Manager cfg resp
         -> ID (Instance cfg)
         -> m State
getState (InstanceManager ref _ _) iid = liftIO $ do
  withMVarMasked ref $ pure . maybe Stopped instanceState . M.lookup (fromId iid)

-- | Send a command to the given instance, if it's running.
--   Returns @True@ if the instance was running, otherwise @False@.
sendCommand :: MonadIO m
            => Manager cfg res
            -> ID (Instance cfg)
            -> Command
            -> m Bool
sendCommand (InstanceManager ref _ _) iid cmd =
  liftIO $ withMVarMasked ref $ \m -> do
    case M.lookup (fromId iid) m of
      Just is -> post (instanceHandle is) cmd >> pure True
      _       -> pure False

-- | Run the given instance computation in the context for the given instance,
--   if the instance is currently running.
--   Returns @Nothing@ if the instance was not running.
withInstance :: (MonadIO m, ServerConfig cfg)
             => Manager cfg resp
             -> ID (Instance cfg)
             -> Inst cfg resp a
             -> m (Maybe a)
withInstance mgr@(InstanceManager ref _ conn) iid act = do
  liftIO $ withMVarMasked ref $ \m -> do
    case M.lookup (fromId iid) m of
      Just is -> Just <$> shell_ (runInst (instanceInfo is) act)
      Nothing -> pure Nothing

-- | Like 'withInstance', but discards its result.
withInstance_ :: (MonadIO m, ServerConfig cfg)
              => Manager cfg resp
              -> ID (Instance cfg)
              -> Inst cfg resp a
              -> m ()
withInstance_ mgr iid act = void $ withInstance mgr iid act
