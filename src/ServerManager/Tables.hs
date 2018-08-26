{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -O0 #-}
module ServerManager.Tables
  ( module ServerManager.Model
  , Model (..), model
  ) where
import Data.Typeable
import Database.Selda
import ServerManager.Model
import ServerManager.Log.Format

data Model cfg = Model
  { users         :: Table User
  , servers       :: Table Server
  , configs       :: Table cfg
  , instances     :: Table (Instance cfg)
  , backups       :: Table (Backup cfg)
  , players       :: Table (Player cfg)
  , logs          :: Table (LogMessage cfg)
  , apiKeys       :: Table ApiKey
  , instancePerms :: Table (InstancePermission cfg)
  }

model :: ( Relational cfg
         , HasField "id" cfg
         , FieldType "id" cfg ~ ID cfg
         ) => Proxy cfg -> Model cfg
model _ = m
  where
    m = Model
      { users = table "users"
        [ #id          :- autoPrimary
        , #userName    :- index
        , #masterKeyId :- foreignKey (apiKeys m) #id
        ]
      , apiKeys = table "apiKeys"
        [ #id      :- autoPrimary
        , #ownerId :- foreignKey (users m) #id
        , #ownerId :- index
        , #key     :- unique
        ]
      , instancePerms = table "instancePerms"
        [ #instanceId :- index
        , #keyId      :- index
        , #instanceId :- foreignKey (instances m) #id
        , #keyId      :- foreignKey (apiKeys m) #id
        ]
      , servers = table "servers"
        [ #id :- autoPrimary
        ]
      , configs = table "configs"
        [ #id :- autoPrimary
        ]
      , instances = table "instances"
        [ #id       :- autoPrimary
        , #serverId :- foreignKey (servers m) #id
        , #serverId :- index
        ]
      , backups = table "backups"
        [ #id :- autoPrimary
        ]
      , players = table "players"
        [ #playerName :- primary
        , #userId     :- foreignKey (users m) #id
        , #instanceId :- foreignKey (instances m) #id
        , #instanceId :- index
        , #userId     :- index
        ]
      , logs = table "logs"
        [ #id :- autoPrimary
        , #logInstance :- foreignKey (instances m) (#id)
        , #logUser :- foreignKey (users m) (#id)
        , #logPlayer :- foreignKey (players m) (#playerName)
        , #logInstance :- index
        , #logTimestamp :- index
        , #logUser :- index
        , #logPlayer :- index
        ]
      }
