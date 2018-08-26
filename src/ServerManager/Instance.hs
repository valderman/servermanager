{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds #-}
{-# LANGUAGE FlexibleContexts, OverloadedLabels #-}
module ServerManager.Instance
  ( module ServerManager.Instance.Monad
  , State (..)
  , ServerArgs, args
  , setState
  , getServer, getConfig, getArgs, getWorkingDirectory, getExecutable
  , ban, unban, whitelist, unWhitelist, setOp
  ) where
import Control.Shell hiding (try, ask, when)
import qualified Control.Shell as Shell
import Control.Concurrent
import Control.Monad (when)
import qualified Data.IntMap as M
import Data.Text (pack, unpack)
import Database.Selda
import ServerManager.Instance.Command
import ServerManager.Instance.Manager.Type
import ServerManager.Instance.Monad
import ServerManager.Instance.Supervisor hiding (Event (..))
import ServerManager.Log
import ServerManager.Model.ServerArgs
import ServerManager.Paths
import ServerManager.ServerConfig
import ServerManager.Tables

-- | Update any number of fields for the given player.
updatePlayer :: MonadInst cfg resp m
             => Text -> [Assignment s (Player cfg)] -> m ()
updatePlayer name updates = liftInst $ do
  ps <- asks (players . infoModel)
  update_ ps (#playerName `is` name) $ \inst -> inst `with` updates

-- | Ban the given player until the given date.
ban :: MonadInst cfg resp m => Text -> Maybe UTCTime -> m ()
ban name expires = updatePlayer name
  [ #isBanned := true
  , #banExpires := literal expires
  ]

-- | Unban the given player.
unban :: MonadInst cfg resp m => Text -> m ()
unban name = updatePlayer name
  [ #isBanned := false
  , #banExpires := null_
  ]

-- | Whitelist the given player until the given date.
whitelist :: MonadInst cfg resp m => Text -> Maybe UTCTime -> m ()
whitelist name expires = updatePlayer name
  [ #isWhiteListed := true
  , #whitelistExpires := literal expires
  ]

-- | Unban the given player.
unWhitelist :: MonadInst cfg resp m => Text -> m ()
unWhitelist name = updatePlayer name
  [ #isWhiteListed := false
  , #whitelistExpires := null_
  ]

-- | Set the operator flag for the given player.
setOp :: MonadInst cfg resp m => Text -> Bool -> m ()
setOp name op = updatePlayer name [ #isOp := literal op ]

-- | Update the instance's current state.
setState :: MonadInst cfg resp m => State -> m ()
setState state = liftInst $ do
    iid <- asks infoId
    modInsts <- asks infoModifyInstances
    modInsts $ M.update f (fromId iid)
  where
    f is = Just $ is { instanceState = state }

-- | Get the server configuration for the current instance.
getServer :: MonadInst cfg resp m => m Server
getServer = liftInst $ do
  iid <- asks infoId
  insts <- asks (instances . infoModel)
  svrs <- asks (servers . infoModel)
  fmap head . query $ do
    sid <- #serverId `from` select insts `suchThat` (#id `is` iid)
    server <- select svrs
    restrict (server ! #id .== sid)
    return server

-- | Get the configuration for the current instance.
getConfig :: (ServerConfig cfg, MonadInst cfg resp m) => m cfg
getConfig = liftInst $ do
  iid <- asks infoId
  insts <- asks (instances . infoModel)
  cfgs <- asks (configs . infoModel)
  fmap head . query $ do
    sid <- #configId `from` select insts `suchThat` (#id `is` iid)
    cfg <- select cfgs
    restrict (cfg ! #id .== sid)
    return cfg

-- | Get the arguments (both server global and instance specific) for the
--   current instance.
getArgs :: MonadInst cfg resp m => m ServerArgs
getArgs = liftInst $ do
  iid <- asks infoId
  insts <- asks (instances . infoModel)
  svrs <- asks (servers . infoModel)
  [sargs :*: iargs] <- query $ do
    inst <- select insts `suchThat` (#id `is` iid)
    server <- select svrs
    restrict (server ! #id .== inst ! #serverId)
    return (server ! #arguments :*: inst ! #arguments)
  return $ concatArgs [sargs, iargs]

-- | Get the current instance's working directory.
getWorkingDirectory :: MonadInst cfg resp m => m FilePath
getWorkingDirectory = liftInst $ do
  info <- ask
  pure $ workingDirectoryFor (infoRootPath info) (infoId info)

-- | Get the current instance's working directory.
getExecutable :: MonadInst cfg resp m => m FilePath
getExecutable = liftInst $ do
  info <- ask
  cfg <- getServer
  pure $ executablePathFor (infoRootPath info)
                           (infoId info)
                           (unpack $ executable cfg)
