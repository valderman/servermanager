{-# LANGUAGE GADTs #-}
module ServerManager.Instance.Manager.Type where
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.IntMap as M
import Database.Selda.Backend (SeldaConnection)
import ServerManager.Instance.Command
import ServerManager.Instance.Supervisor
import ServerManager.Log.Format

data State
  = Stopped
  | Stopping
  | Starting
  | Running
  deriving (Show, Eq, Ord, Enum, Bounded)

data InstanceState info cfg resp = InstanceState
  { instanceHandle :: InstanceHandle Command resp (LogMessage cfg)
  , instanceState  :: State
  , instanceInfo   :: info cfg resp
  }

data InstanceManager info svc cfg resp = InstanceManager
  { runningInstances :: MVar (M.IntMap (InstanceState info cfg resp))
  , mgrConfig        :: svc cfg resp
  , mgrConnection    :: SeldaConnection
  }

type InstanceUpdate m info cfg resp =
  (M.IntMap (InstanceState info cfg resp) -> M.IntMap (InstanceState info cfg resp))
  -> m cfg resp ()

-- | Update the list of running instances.
modifyRunningInstances :: MonadIO (m cfg resp)
                       => InstanceManager info svc cfg resp
                       -> InstanceUpdate m info cfg resp
modifyRunningInstances mgr f = liftIO $ do
  modifyMVarMasked_ (runningInstances mgr) $ pure . f
