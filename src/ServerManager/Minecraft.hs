{-# LANGUAGE OverloadedStrings #-}
module ServerManager.Minecraft where
import Data.Text as Text (Text, intercalate, take, length)
import Data.Text.Encoding
import ServerManager
import ServerManager.Minecraft.Config
import ServerManager.Minecraft.ServerProperties

instance ServerConfig Config where
  serializeConfig = encodeUtf8 . serverProperties
  configPath _ = "server.properties"

send' :: MonadInst cfg resp m => [Text] -> m ()
send' = liftInst . send . intercalate " "

type TriggeredM = Triggered (LogMessage Config) (Inst Config (LogMessage Config))

awaitMsg :: Text -> TriggeredM ()
awaitMsg s = await $ \e ->
  s == Text.take (Text.length s) (logMessage e)

triggers :: Triggers Config (LogMessage Config)
triggers = defaultTriggers
  { startTrigger = trigger $ do
      awaitMsg "Starting minecraft server"
      awaitMsg "Time elapsed:"
      awaitMsg"Done ("
      setState Running
  , restartTrigger = startTrigger triggers
  , backupTrigger = \backupFiles -> trigger $ do
      logItem $ item Info Manager "Initiating backup"
      send "save-off"
      awaitMsg "Automatic saving is now disabled"
      send "save-all flush"
      awaitMsg "Saved the game"
      liftInst backupFiles
      send "save-on"
      awaitMsg "Automatic saving is now enabled"
      logItem $ item Info Manager "Backup completed"
  , broadcastTrigger = \msg -> trigger $ do
      send msg
  , banTrigger = \player _ -> trigger $ do
      send' ["ban", player]
  , unbanTrigger = \player -> trigger $ do
      send' ["unban", player]
  , whitelistTrigger = \player _ -> trigger $ do
      send' ["whitelist", "add", player]
  , unwhitelistTrigger = \player -> trigger $ do
      send' ["whitelist", "remove", player]
  , opTrigger = \player op -> trigger $ do
      send' [if op then "op" else "deop", player]
  }
