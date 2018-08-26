-- | High level backend functionality for the server manager, such as commands
--   for adding players to ban lists both in the config and on running
--   instances, etc.
module ServerManager.Backend
  ( createManager
  , startInstance, stopInstance, getState
  ) where
import Database.Selda
import Database.Selda.Backend (runSeldaT)
import ServerManager.Instance as Live
import ServerManager.Instance.Manager
import ServerManager.ServerConfig
import ServerManager.Tables

-- | Create an instance with the given configuration.
createInstance :: (ServerConfig cfg, MonadSelda m)
               => Manager cfg resp
               -> Instance cfg
               -> m (ID (Instance cfg))
createInstance mgr inst = runSeldaT (mgrConnection mgr) $ do
  return undefined

-- | Ban the given player until the given point in time.
banPlayer :: (ServerConfig cfg, MonadSelda m)
          => Manager cfg resp
          -> ID (Instance cfg)
          -> Text
          -> Maybe UTCTime
          -> m ()
banPlayer mgr iid player expires = do
  withInstance_ mgr iid $ do
    Live.sendCommand (Ban player expires)
    ban player expires

-- | Unban the given player.
unbanPlayer :: (ServerConfig cfg, MonadSelda m)
            => Manager cfg resp -> ID (Instance cfg) -> Text -> m ()
unbanPlayer mgr iid player = do
  withInstance_ mgr iid $ do
    Live.sendCommand (Unban player)
    unban player

-- | Whitelist the given player until the given point in time.
whitelistPlayer :: (ServerConfig cfg, MonadSelda m)
                => Manager cfg resp
                -> ID (Instance cfg)
                -> Text
                -> Maybe UTCTime
                -> m ()
whitelistPlayer mgr iid player expires = do
  withInstance_ mgr iid $ do
    Live.sendCommand (WhiteList player expires)
    whitelist player expires

-- | Remove the given player from the whitelist.
unwhitelistPlayer :: (ServerConfig cfg, MonadSelda m)
                  => Manager cfg resp -> ID (Instance cfg) -> Text -> m ()
unwhitelistPlayer mgr iid player = do
  withInstance_ mgr iid $ do
    Live.sendCommand (UnWhiteList player)
    unWhitelist player
