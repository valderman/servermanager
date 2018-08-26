-- | McMgr event logging.
module ServerManager.Log
  ( module ServerManager.Log.Format
  , logIO
  ) where
import Database.Selda
import Database.Selda.Backend (runSeldaT, SeldaConnection)
import Data.Time (getCurrentTime)
import ServerManager.Log.Format
import ServerManager.Tables

logIO :: Typeable cfg
      => Model cfg
      -> SeldaConnection
      -> ID (Instance cfg)
      -> LogMessage cfg
      -> IO ()
logIO m c inst message = do
  now <- getCurrentTime
  flip runSeldaT c $ insert_ (logs m) [message
    { logTimestamp = now
    , logInstance = inst
    }]
