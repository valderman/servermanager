{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
module ServerManager.Model
  ( Typeable
  , User (..)
  , Instance (..)
  , Server (..), ServerArgs (..)
  , Player (..)
  , Backup (..)
  , ApiKey (..), Permission (..), InstancePermission (..)
  ) where
import Prelude hiding (id)
import Data.Typeable
import Data.Char (isSpace)
import qualified Data.Text as Text
import Database.Selda
import Database.Selda.SqlType
import ServerManager.Model.Permission
import ServerManager.Model.ServerArgs

data User = User
  { id          :: ID User
  , masterKeyId :: ID ApiKey
  , userName    :: Text
  , password    :: Text
  , salt        :: Text
  } deriving Generic
instance SqlRow User

data ApiKey = ApiKey
  { id      :: ID ApiKey
  , ownerId :: ID User
  , key     :: Text
  , isAdmin :: Bool
  } deriving Generic
instance SqlRow ApiKey

data InstancePermission cfg = InstancePermission
  { instanceId :: ID (Instance cfg)
  , keyId      :: ID ApiKey
  , permission :: Permission
  } deriving Generic
instance Typeable cfg => SqlRow (InstancePermission cfg)

data Instance cfg = Instance
  { id               :: ID (Instance cfg)
  , configId         :: ID cfg
  , serverId         :: ID Server
  , arguments        :: ServerArgs
  , gracefulShutdown :: Bool
  , retainedBackups  :: Int
  , backupEveryNDays :: Maybe Int
  } deriving Generic
instance Typeable cfg => SqlRow (Instance cfg)

data Server = Server
  { id          :: ID Server
  , title       :: Text
  , description :: Text
  , executable  :: Text
  , arguments   :: ServerArgs
  } deriving Generic
instance SqlRow Server

data Backup cfg = Backup
  { id         :: ID (Backup cfg)
  , instanceId :: ID (Instance cfg)
  , timestamp  :: UTCTime
  , hash       :: Text
  } deriving Generic
instance Typeable cfg => SqlRow (Backup cfg)

data Player cfg = Player
  { playerName       :: Text
  , instanceId       :: ID (Instance cfg)
  , userId           :: Maybe (ID User)
  , isOp             :: Bool
  , isWhiteListed    :: Bool
  , isBanned         :: Bool
  , banExpires       :: Maybe UTCTime
  , whitelistExpires :: Maybe UTCTime
  } deriving Generic
instance Typeable cfg => SqlRow (Player cfg)
