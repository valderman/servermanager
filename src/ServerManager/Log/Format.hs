{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}
-- | Format of ServerManager log messages.
module ServerManager.Log.Format
  ( Typeable, LogMessage (..) , LogType (..), LogSource (..)
  , item
  ) where
import Prelude hiding (id)
import Database.Selda
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Database.Selda.SqlType
import qualified Data.Text as Text
import Data.Time
import ServerManager.Model (Typeable, Instance, User)

data LogType = Info | Warn | Severe
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
instance SqlType LogType
instance SqlOrd LogType

data LogSource = Manager | Server | Mod Text
  deriving (Eq, Ord, Show)

instance SqlType LogSource where
  mkLit = LCustom . LText . logSourceLit
  sqlType _ = TText
  defaultValue = mkLit Manager
  fromSql (SqlString "Manager")              = Manager
  fromSql (SqlString "Server")               = Server
  fromSql (SqlString s) | Text.head s == ':' = Mod (Text.tail s)
  fromSql _                                  = error "Not a LogSource!"

logSourceLit :: LogSource -> Text
logSourceLit Manager = "Manager"
logSourceLit Server  = "Server"
logSourceLit (Mod m) = ":" <> m

data LogMessage cfg = LogMessage
  { id           :: ID (LogMessage cfg)
  , logInstance  :: ID (Instance cfg)
  , logType      :: LogType
  , logSource    :: LogSource
  , logTimestamp :: UTCTime
  , logMessage   :: Text
  , logUser      :: Maybe (ID User)
  , logPlayer    :: Maybe Text
  } deriving (Generic, Show)
instance Typeable cfg => SqlRow (LogMessage cfg)

-- | Create a log item with the given type, source and message.
item :: Typeable cfg => LogType -> LogSource -> Text -> LogMessage cfg
item t s m = LogMessage
  { id = def
  , logInstance = def
  , logType = t
  , logSource = s
  , logTimestamp = def
  , logMessage = m
  , logUser = Nothing
  , logPlayer = Nothing
  }
