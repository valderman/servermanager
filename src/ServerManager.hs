-- | Configurable server manager for multiplayer game servers.
module ServerManager
  ( ServiceConfig (..), serviceConfig, defaultTriggers
  , Triggered, Trigger, lift, event, trigger
  , await, awaitM, awaitVal
  , send
  , State (..)
  , ServerConfig (..)
  , module ServerManager.Instance.Command
  , module ServerManager.Log.Format
  , module ServerManager.Instance
  ) where
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Text as Text (Text, concat)
import Control.Monad.Triggered
import ServerManager.Instance hiding
  ( addToInstanceManager, removeFromInstanceManager, setGracefulShutdown
  , runInst, InstanceInfo (..)
  )
import ServerManager.Instance.Command
import ServerManager.Log.Format
import ServerManager.ServerConfig
import ServerManager.ServiceConfig

-- | Send a plain text command to the server.
send :: MonadInst cfg resp m => Text -> m ()
send = liftInst . sendCommand . TextCommand
