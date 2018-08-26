{-# LANGUAGE OverloadedStrings #-}
module ServerManager.Model.Permission where
import qualified Data.Text as T
import Database.Selda
import Database.Selda.SqlType

data Permission
  = Admin
  | Manage      -- List, start, stop, backup
  | Users       -- Manage bans and whitelists
  | Ops         -- Op, deop
  | None        -- No permissions
  | Custom Text -- Run custom command
    deriving (Show, Ord, Eq)

instance SqlType Permission where
  sqlType _ = TText
  defaultValue = mkLit None
  mkLit = LCustom . mkLit . showPerm
  fromSql (SqlString s) = readPerm s
  fromSql x             = error $ "Permission not a Permission: " ++ show x

showPerm :: Permission -> Text
showPerm Admin      = "Admin"
showPerm Manage     = "Manage"
showPerm Users      = "Users"
showPerm Ops        = "Ops"
showPerm None       = ""
showPerm (Custom s) = ":" <> s

readPerm :: Text -> Permission
readPerm "Admin"    = Admin
readPerm "Manage"   = Manage
readPerm "Users"    = Users
readPerm "Ops"      = Ops
readPerm ""         = None
readPerm s
  | T.head s == ':' = Custom (T.tail s)
  | otherwise       = error $ "Permission not a Permission: " ++ show s
