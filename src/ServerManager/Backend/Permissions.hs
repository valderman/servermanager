{-# LANGUAGE KindSignatures, DataKinds, OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module ServerManager.Backend.Permissions
  ( AuthInfo (..), Permission (..)
  , ServerConfig, Check (..)
  ) where
import Data.Proxy
import Database.Selda
import ServerManager.Tables
import ServerManager.ServerConfig

data AuthInfo cfg (p :: Permission) = AuthInfo
  { apiKey    :: Text
  , instId    :: Maybe Int
  , customCmd :: Maybe Text
  }

modelFor :: forall cfg p. ServerConfig cfg => AuthInfo cfg p -> Model cfg
modelFor _ = model (Proxy @cfg)

iidFor :: AuthInfo cfg p -> Maybe (ID (Instance cfg))
iidFor = fmap toId . instId

withPerm :: forall p' p cfg. AuthInfo cfg p -> AuthInfo cfg p'
withPerm ai = AuthInfo
  { apiKey = apiKey ai
  , instId = instId ai
  , customCmd = customCmd ai
  }

class Check (p :: Permission) where
  -- | Checks that the given API key has the given permission for the given
  --   instance ID.
  check :: (ServerConfig cfg, MonadSelda m) => AuthInfo cfg p -> m Bool

instancePermQuery :: ServerConfig cfg
                  => Model cfg
                  -> Text
                  -> ID (Instance cfg)
                  -> Permission
                  -> Query s (Col s Bool)
instancePermQuery m key iid perm = do
  k <- select (apiKeys m) `suchThat` (#key `is` key)
  let ips = #keyId `from` select (instancePerms m) `suchThat` \ip ->
        (ip ! #instanceId .== literal iid) .&&
        (ip ! #permission .== literal perm)
  return (k ! #isAdmin .|| k ! #id `isIn` ips)

instancePerm :: (ServerConfig cfg, MonadSelda m)
             => Model cfg
             -> Text
             -> ID (Instance cfg)
             -> Permission
             -> m Bool
instancePerm m key iid perm = do
  [ok] <- query $ instancePermQuery m key iid perm
  pure ok

checkFor :: (ServerConfig cfg, MonadSelda m)
         => Permission
         -> AuthInfo cfg p
         -> m Bool
checkFor p ai
  | Just iid <- iidFor ai = instancePerm (modelFor ai) (apiKey ai) iid p
  | otherwise             = check (withPerm @Admin ai)

instance Check Admin where
  check ai = do
    [match] <- query $ do
      let adminKeys =
            #key `from` select (apiKeys $ modelFor ai) `suchThat` (! #isAdmin)
      return (text (apiKey ai) `isIn` adminKeys)
    return match

instance Check Manage where
  check = checkFor Manage

instance Check Users where
  check = checkFor Users

instance Check Ops where
  check = checkFor Ops

instance Check (Custom t) where
  check ai
    | Just cmd <- customCmd ai = checkFor (Custom cmd) ai
    | otherwise                = check (withPerm @Admin ai)

instance Check None where
  check _ = pure True
