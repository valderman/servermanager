-- | Configuration for the server manager itself.
module ServerManager.ServiceConfig
  ( ServiceConfig (..)
  , serviceConfig, defaultTriggers
  ) where
import Data.ByteString as BS
import Data.Text as Text
import Control.Monad.Triggered
import ServerManager.Instance.Monad
import ServerManager.Instance (State (Running), setState)
import ServerManager.Log.Format
import ServerManager.ServerConfig

data ServiceConfig cfg resp = ServiceConfig
  { -- | Triggers reacting to various events.
    cfgTriggers :: Triggers cfg resp

    -- | Function to turn log messages from the instance into structured
    --   @LogMessage@ entries. If this is not implemented, messages will not
    --   be properly logged, and will not be passed to waiting triggers.
    --   Defaults to @const Nothing@.
  , cfgLogParser :: ByteString -> Maybe (LogMessage cfg)

    -- | Executed after the instance is stopped. The given @Bool@ will be @True@
    --   if the shutdown was graceful, otherwise @False@.
    --   On abnormal shutdown, this trigger is only executed once all attempts
    --   to restart the instance have failed.
    --   Filled in by the user.
  , cfgDeathHandler :: Bool -> Inst cfg resp ()

    -- | Computation to write out a server-readable list of ops, given the
    --   working directory of the current instance and a list of all ops
    --   on the instance.
    --   Defaults to a no-op.
  , cfgWriteOpList :: FilePath -> [Text] -> IO ()

    -- | Computation to write out a server-readable player ban list, given the
    --   working directory of the current instance and a list of all banned
    --   players.
    --   Defaults to a no-op.
  , cfgWriteBanList :: FilePath -> [Text] -> IO ()

    -- | Computation to write out a server-readable player whitelist, given the
    --   working directory of the current instance and a list of all whitelisted
    --   players.
    --   Defaults to a no-op.
  , cfgWriteWhiteList :: FilePath -> [Text] -> IO ()

    -- | The root directory of the server manager.
    --   This is where all configuration files, logs, databases, working data,
    --   backups, etc. will be stored.
    --   Defaults to the current working directory.
  , cfgRootDirectory :: FilePath
  }

-- | The default service configuration.
--   For correct operation, at least 'cfgTriggers' and 'cfgLogParser'
--   should be overriden.
serviceConfig :: ServerConfig cfg => ServiceConfig cfg resp
serviceConfig = ServiceConfig
  { cfgTriggers = defaultTriggers
  , cfgLogParser = const Nothing
  , cfgDeathHandler = const (pure ())
  , cfgWriteOpList = \_ _ -> pure ()
  , cfgWriteBanList = \_ _ -> pure ()
  , cfgWriteWhiteList = \_ _ -> pure ()
  , cfgRootDirectory = "."
  }

-- | The default set of triggers for a service.
--   As a bare minimum, 'stopTrigger' is required for correct operation,
--   although it's strongly recommended that ALL triggers are implemented
--   (with the possible exception of 'logTrigger').
defaultTriggers :: ServerConfig cfg => Triggers cfg resp
defaultTriggers = Triggers
  { startTrigger       = trigger $ lift $ setState Running
  , stopTrigger        = error "stopTrigger is required"
  , restartTrigger     = startTrigger defaultTriggers
  , backupTrigger      = \backup -> trigger $ lift backup >> pure ()
  , broadcastTrigger   = \msg -> unsupportedBcast msg
  , banTrigger         = \_ _ -> unsupported "ban"
  , unbanTrigger       = \_ -> unsupported "unban"
  , whitelistTrigger   = \_ _ -> unsupported "whitelist"
  , unwhitelistTrigger = \_ -> unsupported "unwhitelist"
  , opTrigger          = \_ _ -> unsupported "op"
  , logTrigger         = trigger $ pure ()
  }

unsupportedBcast :: Typeable cfg
                 => Text
                 -> Trigger (LogMessage cfg) (Inst cfg resp) ()
unsupportedBcast msg = trigger $ lift $ logItem $ item Warn Manager $ Text.concat
  [ "No broadcast trigger configured; message will not be sent: "
  , msg
  ]

unsupported :: Typeable cfg
            => Text
            -> Trigger (LogMessage cfg) (Inst cfg resp) ()
unsupported evt = trigger $ lift $ logItem $ item Warn Manager $ Text.concat
  [ "No trigger configured for event '", evt, "'; operation will not "
  , "take effect until the server is restarted."
  ]
