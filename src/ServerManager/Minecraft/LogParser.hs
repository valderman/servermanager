{-# LANGUAGE OverloadedStrings #-}
-- | Parser for Minecraft log messages.
module ServerManager.Minecraft.LogParser (parseMessage, parseMessageAt) where
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import ServerManager.Log.Format

-- | Parse a log message, setting the time stamp to the start of the
--   UNIX epoch.
parseMessage :: ByteString -> LogMessage
parseMessage = parseMessageAt timeZero

-- | Parse a log message and assign it the given time stamp.
parseMessageAt :: UTCTime -> ByteString -> LogMessage
parseMessageAt t bs =
  case parse messageParser bs of
    Partial f
      | Done _ msg <- f "" -> msg {logTimestamp = t}
      | otherwise          -> unknown
    Done _ msg             -> msg {logTimestamp = t}
    _                      -> unknown
  where
    unknown = (item Info Server (decodeUtf8 bs)) { logTimestamp = t }

timeZero :: UTCTime
timeZero = UTCTime (ModifiedJulianDay 0) 0

messageParser :: Parser LogMessage
messageParser = do
  typ <- parseType
  string ": "
  src <- (Mod <$> parseMod) <|> pure Server
  skipSpace
  msg <- decodeUtf8 <$> takeWhile1 (\c -> c /= '\r' &&  c /= '\n')
  pure $ (item typ src msg) { logTimestamp = timeZero }

skipTimeStamp :: Parser ()
skipTimeStamp = do
  skipSpace
  char '['
  skipWhile (/= ']')
  char ']'
  return ()

skipThreadName :: Parser ()
skipThreadName = do
  skipWhile (/= '/')
  char '/'
  return ()

parseType :: Parser LogType
parseType = do
  skipTimeStamp
  skipSpace
  char '['
  skipThreadName
  typ <- takeWhile1 (/= ']')
  char ']'
  pure $ case typ of
    "INFO"   -> Info
    "WARN"   -> Warn
    "SEVERE" -> Severe
    _        -> error $ "Unknown log message type: " ++ show typ

parseMod :: Parser Text
parseMod = do
  char '['
  skipThreadName
  modname <- decodeUtf8 <$> takeWhile1 (/= ']')
  char ']'
  pure modname
