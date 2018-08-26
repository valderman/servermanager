{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, FlexibleInstances, PatternGuards #-}
-- | Per-instance configuration.
module ServerManager.Minecraft.Config where
import Prelude hiding (id)
import Database.Selda
import Database.Selda.SqlType
import Data.Char
import Data.List
import Data.Maybe
import Data.Text as T (pack, unpack, breakOn)
import Text.Read hiding (String, get)

data GameMode = Survival | Creative | Adventure | Spectator
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
instance SqlType GameMode

data Difficulty = Peaceful | Easy | Normal | Hard
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
instance SqlType Difficulty

data LevelType = DefaultType | Flat | LargeBiomes | Amplified | Buffet
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
instance SqlType LevelType

data OpPermissions = ManageTerrain | ManageRules | ManagePlayers
  deriving (Eq, Ord, Enum, Bounded, Read, Show)
instance SqlType OpPermissions

data QuerySettings = QuerySettings { queryPort :: !Int }
  deriving Show

instance SqlType QuerySettings where
  mkLit = LCustom . mkLit
  sqlType _ = TInt
  fromSql (SqlInt port) = QuerySettings port
  fromSql _             = error "Not a QuerySettings"
  defaultValue = mkLit (QuerySettings 0)

data ResourcePack = ResourcePack
  { packURI  :: !Text
  , packSHA1 :: !Text
  } deriving Show

instance SqlType ResourcePack where
  mkLit (ResourcePack uri hash) = LCustom (mkLit (hash <> "|" <> uri))
  sqlType _ = TText
  defaultValue = mkLit (ResourcePack "" "")

  fromSql (SqlString s) | (hash, uri) <- breakOn "|" s =
    ResourcePack {packURI = uri, packSHA1 = hash}
  fromSql _ =
    error "Not a resource pack description"

-- | Per-instance configuration. The following options that are normally
--   present in @server.properties@ have been left out since they either
--   conflict with or are superceded by functionality provided by McMgr,
--   or just didn't seem necessary for some arbitrary reason:
--   * RCon settings
--   * level name
--   * server ip
--   * OP permission level 4 (stopping the server)
--   * native transport on/off
--   * compression threshold
data Config = Config
  { id                   :: ID Config
  , allowSurvivalFlight  :: Bool
  , allowNether          :: Bool
  , announceAchievements :: Bool
  , difficulty           :: Difficulty
  , enableCommandBlock   :: Bool
  , enablePvP            :: Bool
  , enableSnooper        :: Bool
  , enableWhitelist      :: Bool
  , forceGameMode        :: Bool
  , gameMode             :: GameMode
  , generateStructures   :: Bool
  , generatorSettings    :: Text
  , hardcore             :: Bool
  , levelSeed            :: (Maybe Text)
  , levelType            :: LevelType
  , maxBuildHeight       :: Int
  , maxPlayerIdleMins    :: (Maybe Int)
  , maxPlayers           :: Int
  , maxTickMSecs         :: (Maybe Int)
  , maxWorldRadius       :: (Maybe Int)
  , motd                 :: Text
  , onlineMode           :: Bool
  , opPermissionLevel    :: OpPermissions
  , querySettings        :: (Maybe QuerySettings)
  , resourcePack         :: (Maybe ResourcePack)
  , serverPort           :: Int
  , spawnAnimals         :: Bool
  , spawnMonsters        :: Bool
  , spawnNPCs            :: Bool
  , spawnProtectRadius   :: Int
  , viewDistanceChunks   :: Int
  } deriving Generic
instance SqlRow Config

class Property a where
  propType :: a -> PropType
  toProp   :: a -> Text
  fromProp :: Text -> Maybe a

instance Property Bool where
  propType _ = Bool
  toProp True = "true"
  toProp _    = "false"
  fromProp "true"  = Just True
  fromProp "false" = Just False
  fromProp _       = Nothing

instance Property Int where
  propType _ = Int
  toProp = pack . show
  fromProp = readMaybe . unpack

instance Property Text where
  propType _ = Text
  toProp x = x
  fromProp = Just

instance Property a => Property (Maybe a) where
  propType _ = propType (undefined :: a)
  toProp = maybe "" toProp
  fromProp s
    | Just x <- fromProp s = Just (Just x)
    | otherwise            = Nothing

instance Property Difficulty where
  propType _ = OneOf
    [ ("Peaceful", "0")
    , ("Easy", "1")
    , ("Normal", "2")
    , ("Hard", "3")
    ]
  toProp = toProp . fromEnum
  fromProp "0" = Just Peaceful
  fromProp "1" = Just Easy
  fromProp "2" = Just Normal
  fromProp "3" = Just Hard
  fromProp _   = Nothing

instance Property GameMode where
  propType _ = OneOf
    [ ("Survival", "0")
    , ("Creative", "1")
    , ("Adventure", "2")
    , ("Spectator", "3")
    ]
  toProp = toProp . fromEnum
  fromProp "0" = Just Survival
  fromProp "1" = Just Creative
  fromProp "2" = Just Adventure
  fromProp "3" = Just Spectator
  fromProp _   = Nothing

instance Property OpPermissions where
  propType _ = OneOf
    [ ("Ignore spawn protection", "1")
    , ("Manage rules", "2")
    , ("Manage players", "3")
    ]
  toProp = toProp . succ . fromEnum
  fromProp "1" = Just ManageTerrain
  fromProp "2" = Just ManageRules
  fromProp "3" = Just ManagePlayers
  fromProp _   = Nothing

instance Property LevelType where
  propType _ = OneOf
    [ ("Default", "DEFAULT")
    , ("Flat", "FLAT")
    , ("Large biomes", "LARGEBIOMES")
    , ("Amplified", "AMPLIFIED")
    ]
  toProp DefaultType = "DEFAULT"
  toProp Flat        = "FLAT"
  toProp LargeBiomes = "LARGEBIOMES"
  toProp Amplified   = "AMPLIFIED"
  fromProp "DEFAULT"     = Just DefaultType
  fromProp "FLAT"        = Just Flat
  fromProp "LARGEBIOMES" = Just LargeBiomes
  fromProp "AMPLIFIED"   = Just Amplified
  fromProp _             = Nothing

data PropType
  = Bool
  | Int
  | Text
  | OneOf [(Text, Text)]
  | Set [(Text, Text)]
    deriving Show

data Setting = Setting
  { get         :: !(Config -> Text)
  , set         :: !(Config -> Text -> Config)
  , propName    :: !Text
  , humanName   :: !Text
  , settingType :: !PropType
  } | Nested
  { outerSetting   :: !Setting
  , nestedSettings :: !(Config -> Bool, [Setting])
  }

-- | A setting, consisting of a getter, a setter, a @server.properties@
--   identifier, and a human-readable identifier. If the @server.properties@
--   identifier is empty, the setting will not be written to that file.
setting :: forall a.
           Property a
        => (Config -> a)
        -> (Config -> a -> Config)
        -> Text
        -> Text
        -> Setting
setting g s pn hn = Setting
  { get = toProp . g
  , set = \c x -> s c (maybe (g defaultConfig) (\x -> x) (fromProp x))
  , propName = pn
  , humanName = hn
  , settingType = propType (undefined :: a)
  }

-- | All user-modifiable settings.
settings :: [Setting]
settings =
  [ setting allowSurvivalFlight
            (\c x -> c {allowSurvivalFlight = x})
            "allow-flight"
            "Allow flight in survival mode"
  , setting allowNether
            (\c x -> c {allowNether = x})
            "allow-nether"
            "Allow nether"
  , setting announceAchievements
            (\c x -> c {announceAchievements = x})
            "announce-player-achievements"
            "Announce player achievements"
  , setting difficulty
            (\c x -> c {difficulty = x})
            "difficulty"
            "Difficulty"
  , setting enableCommandBlock
            (\c x -> c {enableCommandBlock = x})
            "enable-command-block"
            "Enable command block"
  , setting enablePvP
            (\c x -> c {enablePvP = x})
            "pvp"
            "Enable PvP"
  , setting enableSnooper
            (\c x -> c {enableSnooper = x})
            "snooper-enabled"
            "Enable snooper"
  , setting enableWhitelist
            (\c x -> c {enableWhitelist = x})
            "white-list"
            "Enable whitelist"
  , setting forceGameMode
            (\c x -> c {forceGameMode = x})
            "force-gamemode"
            "Force game mode"
  , setting gameMode
            (\c x -> c {gameMode = x})
            "gamemode"
            "Game mode"
  , setting generateStructures
            (\c x -> c {generateStructures = x})
            "generate-structures"
            "Generate structures"
  , setting hardcore
            (\c x -> c {hardcore = x})
            "hardcore"
            "Hardcore"
  , setting levelSeed
            (\c x -> c {levelSeed = x})
            "level-seed"
            "Level seed"
  , setting levelType
            (\c x -> c {levelType = x})
            "level-type"
            "Level type"
  , setting maxBuildHeight
            (\c x -> c {maxBuildHeight = x})
            "max-build-height"
            "Maximum build height"
  , setting maxPlayerIdleMins
            (\c x -> c {maxPlayerIdleMins = x})
            "player-idle-timeout"
            "Maximum build height"
  , setting maxPlayers
            (\c x -> c {maxPlayers = x})
            "max-players"
            "Maximum players"
  , setting maxTickMSecs
            (\c x -> c {maxTickMSecs = x})
            "max-tick-time"
            "Tick time limit (ms)"
  , setting maxWorldRadius
            (\c x -> c {maxWorldRadius = x})
            "max-world-size"
            "Maximum world radius"
  , setting motd
            (\c x -> c {motd = x})
            "motd"
            "Message of the day"
  , setting onlineMode
            (\c x -> c {onlineMode = x})
            "online-mode"
            "Online mode"
  , setting opPermissionLevel
            (\c x -> c {opPermissionLevel = x})
            "op-permission-level"
            "Operator permission level"
  , setting (isJust . querySettings)
            (\c x -> c {querySettings = if x then Just (QuerySettings 0) else Nothing})
            "enable-query"
            "Enable GameSpy4 query server"
    `Nested`
      ( isJust . querySettings
      , [ setting (maybe 0 queryPort . querySettings)
                  (\c x -> if isJust (querySettings c)
                             then c {querySettings = Just $ QuerySettings x}
                             else c)
                  "query.port"
                  "Query server port"
        ]
      )

  , setting (isJust . resourcePack)
            (\c x -> c {resourcePack = if x then Just (ResourcePack "" "") else Nothing})
            ""
            "Enable resource pack"
    `Nested`
    ( isJust . resourcePack
    , [ setting (maybe "" packURI . resourcePack)
                (\c x -> c {resourcePack = do
                               p <- resourcePack c
                               pure (p {packURI = x})
                           })
                "resource-pack"
                "Resource pack URI"
      , setting (maybe "" packSHA1 . resourcePack)
                (\c x -> c { resourcePack = do
                               p <- resourcePack c
                               pure (p {packSHA1 = x})
                           })
                "resource-pack-sha1"
                "Resource pack SHA1 hash"
      ]
    )
  , setting serverPort
            (\c x -> c {serverPort = x})
            "server-port"
            "Server port"
  , setting spawnAnimals
            (\c x -> c {spawnAnimals = x})
            "spawn-animals"
            "Animals"
  , setting spawnMonsters
            (\c x -> c {spawnMonsters = x})
            "spawn-monsters"
            "Monsters"
  , setting spawnNPCs
            (\c x -> c {spawnNPCs = x})
            "spawn-npcs"
            "NPCs"
  , setting spawnProtectRadius
            (\c x -> c {spawnProtectRadius = x})
            "spawn-protect"
            "Spawn protection radius"
  , setting viewDistanceChunks
            (\c x -> c {viewDistanceChunks = x})
            "view-distance"
            "View distance"
  ]

-- | Default configuration for new instances. Differs from Minecraft's default
--   configuration on the following points:
--   * MOTD is set to \"Powered by McMgr!\"
--   * OPs can't stop the server
--   * snooper is disabled
defaultConfig :: Config
defaultConfig = Config
  { id = invalidId
  , allowSurvivalFlight = False
  , allowNether = True
  , announceAchievements = True
  , difficulty = Easy
  , enableCommandBlock = False
  , enablePvP = True
  , enableSnooper = False
  , enableWhitelist = False
  , forceGameMode = False
  , gameMode = Survival
  , generateStructures = True
  , generatorSettings = ""
  , hardcore = False
  , levelSeed = Nothing
  , levelType = DefaultType
  , maxBuildHeight = 256
  , maxPlayerIdleMins = Nothing
  , maxPlayers = 20
  , maxTickMSecs = Just 60000
  , maxWorldRadius = Nothing
  , motd = "Powered by McMgr!"
  , onlineMode = True
  , opPermissionLevel = ManagePlayers
  , querySettings = Nothing
  , resourcePack = Nothing
  , serverPort = 25565
  , spawnAnimals = True
  , spawnMonsters = True
  , spawnNPCs = True
  , spawnProtectRadius = 16
  , viewDistanceChunks = 10
  }
