-- | The commands an instance accepts.
module ServerManager.Instance.Command (Command (..)) where
import Data.Text
import Data.Time

data Command
  = Ban Text (Maybe UTCTime)
  | Unban Text
  | Op Text
  | Unop Text
  | WhiteList Text (Maybe UTCTime)
  | UnWhiteList Text
  | Broadcast Text
  | Backup
  | TextCommand Text
