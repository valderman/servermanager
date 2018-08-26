{-# LANGUAGE OverloadedStrings #-}
-- | @server.properties@ serialization and deserialization.
module ServerManager.Minecraft.ServerProperties where
import ServerManager.Minecraft.Config
import Data.Text as Text hiding (map, concatMap, foldr)

-- | Unchangeable @server.properties@ settings.
constLines :: [Text]
constLines =
    [ "enable-rcon=false"
    , "rcon.password="
    , "rcon.port=25575"
    , "level-name=world"
    , "server-ip="
    , "use-native-transport=true"
    , "network-compression-threshold=256"
    ]

-- | Lines from configuration and the global setting definition.
configLines :: Config -> [Text]
configLines cfg =
  [ Text.concat [propName s', "=", get s' cfg]
  | s <- settings
  , s' <- flatten s
  , not . Text.null $ propName s'
  ]

-- | Flatten a potentially nested setting.
flatten :: Setting -> [Setting]
flatten (Nested top (_, nest)) = concatMap flatten $ top : nest
flatten s                      = [s]

-- | Build a @server.properties@ file consisting of all configurable as well
--   as unconfigurable lines.
serverProperties :: Config -> Text
serverProperties cfg = Text.unlines $ configLines cfg ++ constLines

-- | Read a @server.properties@ file into a config.
readServerProperties :: Text -> Config
readServerProperties s = foldr (.) (\x -> x) configs defaultConfig
  where
    splitkv = breakOn "="
    configs =
      [ flip (set s') val
      | (key, val) <- map splitkv $ Text.lines s
      , s <- settings
      , s' <- flatten s
      , key == propName s'
      ]
