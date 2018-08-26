{-# LANGUAGE PatternGuards #-}
-- | Type and utils for storing lists of command line arguments in a Selda
--   database.
module ServerManager.Model.ServerArgs
  ( ServerArgs
  , args, parseArgs, concatArgs
  ) where
import Control.Applicative
import Data.Char (isSpace)
import Database.Selda
import Database.Selda.SqlType
import Data.Attoparsec.Text as A
import qualified Data.Text as Text

-- | A list of server command line arguments.
newtype ServerArgs = ServerArgs { args :: [Text] }
  deriving Eq

serverArgs :: Parser [Text]
serverArgs = mixedString `sepBy` skipSpace

quotedString :: Parser Text
quotedString = do
  c <- satisfy (`elem` ['"', '\''])
  case c of
    '"'  -> A.takeWhile (/= '"') <* char '"'
    '\'' -> A.takeWhile (/= '\'') <* char '\''

unquotedString :: Parser Text
unquotedString = A.takeWhile1 (notInClass [' ', '\t', '"', '\''])

mixedString :: Parser Text
mixedString = Text.concat <$> many1 (quotedString <|> unquotedString)

-- | Concatenate the given argument lists.
concatArgs :: [ServerArgs] -> ServerArgs
concatArgs = ServerArgs . Prelude.concat . map args

quote :: Text -> Text
quote s
  | Text.any isSpace s = Text.concat ["'", s, "'"]
  | otherwise          = s

-- | Parse a list of command line arguments into a @ServerArgs@ structure.
--   Note that arguments must not contain any @NUL@ characters.
parseArgs :: Text -> Maybe ServerArgs
parseArgs s
  | "\0" `Text.isInfixOf` s =
    Nothing -- TODO: escape NULs instead?
  | otherwise =
    case parse serverArgs s of
      Partial f
        | Done _ args <- f "" -> Just $ ServerArgs args
        | otherwise           -> Nothing
      Done _ args           -> Just $ ServerArgs args

instance Show ServerArgs where
  show = unwords . map (Text.unpack . quote) . args

instance SqlType ServerArgs where
  sqlType _ = TText
  defaultValue = LCustom (defaultValue :: Lit Text)
  mkLit = LCustom . mkLit . Text.intercalate "\0" . args
  fromSql (SqlString s) = ServerArgs $ Text.splitOn "\0" s
  fromSql x             = error $ "ServerArgs not a ServerArgs: " ++ show x
