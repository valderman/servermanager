-- | Functionality to resolve file system paths.
module ServerManager.Paths
  ( workingDirectoryFor
  , executablePathFor
  , backupDirectoryFor
  ) where
import Control.Shell
import Database.Selda (ID, fromId)
import ServerManager.Model

setupDirectories :: FilePath -> Shell ()
setupDirectories root = void $ do
  try $ mkdir True (root </> "instances")
  try $ mkdir True (root </> "servers")
  try $ mkdir True (root </> "backups")

workingDirectoryFor :: FilePath -> ID (Instance cfg) -> FilePath
workingDirectoryFor root iid =
  root </> "instances" </> show (fromId iid)

executablePathFor :: FilePath -> ID (Instance cfg) -> FilePath -> FilePath
executablePathFor root iid exe =
  root </> "servers" </> show (fromId iid) </> exe

backupDirectoryFor :: FilePath -> ID (Instance cfg) -> FilePath
backupDirectoryFor root iid =
  root </> "backups" </> show (fromId iid)
