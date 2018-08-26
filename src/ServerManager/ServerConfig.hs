{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- | Type class for instance configurations.
module ServerManager.ServerConfig
  ( ServerConfig (..)
  , Proxy (..)
  , SqlRow (..)
  ) where
import Data.Proxy
import Data.ByteString (ByteString)
import Database.Selda

class ( HasField "id" cfg
      , FieldType "id" cfg ~ ID cfg
      , SqlRow cfg
      ) => ServerConfig cfg where
  -- | Produces a server-readable string representation of the configuration.
  serializeConfig :: cfg -> ByteString

  -- | Path of the config file, relative to the server root directory.
  configPath :: Proxy cfg -> FilePath
