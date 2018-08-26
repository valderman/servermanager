{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module ServerManager.Backend.Monad where
import Control.Monad.Catch
import Control.Monad.Reader
import Database.Selda
import Database.Selda.Backend
import ServerManager.Backend.Permissions
import ServerManager.Instance.Manager

newtype Backend cfg resp (p :: Permission) a = B
  { unB :: ReaderT (Manager cfg resp) SeldaM a
  } deriving
  ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask
  )

instance MonadSelda (Backend cfg resp p) where
  seldaConnection = B $ asks mgrConnection
  invalidateTable = B . lift . invalidateTable
  wrapTransaction (B commit) (B rollback) (B act) = B $ do
    env <- ask
    lift $ wrapTransaction (runReaderT commit env)
                           (runReaderT rollback env)
                           (runReaderT act env)

runBackend :: (ServerConfig cfg, Check p)
           => Manager cfg resp
           -> AuthInfo cfg p
           -> Backend cfg resp p a
           -> IO (Maybe a)
runBackend mgr ai (B m) = flip runSeldaT (mgrConnection mgr) $ do
  hasPermissions <- check ai
  if hasPermissions
    then Just <$> runReaderT m mgr
    else pure Nothing
