-- | Utilities for performing backups.
module ServerManager.Instance.Backup
  ( backupFiles
  ) where
import Control.Shell as S
import Data.Text (Text, pack)
import Data.Time
import Data.Typeable
import ServerManager.Instance
import ServerManager.Log.Format
import ServerManager.Paths

-- | Back up all files for the current instance.
--   Returns @Just error_message@ if the backup failed for some reason.
backupFiles :: Typeable cfg => Inst cfg resp Bool
backupFiles = do
  iid <- asks infoId
  root <- asks infoRootPath
  let instDir = workingDirectoryFor root iid
      backupDir = backupDirectoryFor root iid
  now <- liftIO getCurrentTime
  result <- liftSh $ S.try $ cpdir instDir (backupDir </> show now)
  case result of
    Left e -> do
      logItem $ item Severe Manager $ "Backup failed: " <> pack e
      return False
    _      -> do
      return True
