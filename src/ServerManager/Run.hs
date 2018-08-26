module ServerManager.Run where
import ServerManager.Instance.Manager
import ServerManager.ServiceConfig

serverManager :: ServiceConfig cfg resp -> IO ()
serverManager cfg = do
  undefined
