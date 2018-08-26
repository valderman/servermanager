{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses #-}
-- | Monad for expressing triggered computations that can be advanced
--   step by step.
module Control.Monad.Triggered
  ( Triggered, Trigger (..)
  , awaitM, await, awaitVal
  , lift, event, trigger
  , step, steps, step0, stepAll, stepAll0
  , isDone, isWaiting, result
  ) where
import Control.Monad.IO.Class
import Control.Monad.Trans

data Triggered e m a where
  Await :: (e -> m Bool) -> Triggered e m ()
  Lift  :: m a -> Triggered e m a
  Bind  :: Triggered e m a -> (a -> Triggered e m b) -> Triggered e m b
  Event :: Triggered e m e
  Pure  :: a -> Triggered e m a

instance Monad m => Functor (Triggered e m) where
  fmap f m = m >>= return . f

instance Monad m => Applicative (Triggered e m) where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return (f x)

instance Monad m => Monad (Triggered e m) where
  return = Pure
  (>>=) = Bind

instance MonadIO m => MonadIO (Triggered e m) where
  liftIO = Lift . liftIO

instance MonadTrans (Triggered e) where
  lift = Lift

instance Show a => Show (Trigger e m a) where
  show (Done a) = "Done " ++ show a
  show (Wait m) = "Wait <continuation>"

-- | A trigger can be in one of two states: done or waiting.
data Trigger e m a
  = Done a
  | Wait (Triggered e m a)

-- | Create a new trigger from a 'Triggered' computation.
trigger :: Triggered e m a -> Trigger e m a
trigger = Wait

-- | Gets the current event.
event :: Triggered e m e
event = Event

isDone :: Trigger e m a -> Bool
isDone (Done _) = True
isDone _        = False

isWaiting :: Trigger e m a -> Bool
isWaiting (Wait _) = True
isWaiting _        = False

result :: Trigger e m a -> a
result (Done x) = x
result _        = error "computation has not yet finished"

-- | Step the given input using the given trigger.
--   If the trigger is in the @Done@ state, this is a no-op.
--
--   Stepping the trigger will advance at most one 'await' call, even if the
--   event would fulfill the condition for more than one consecutive call.
--   To advance past all such calls, use 'steps'.
step :: Monad m => e -> Trigger e m a -> m (Trigger e m a)
step e (Wait m)   = snd <$> step' 1 e m
step e t@(Done _) = pure t

-- | Like 'step', but will proceed past all consecutive 'await' calls that
--   match the given event.
steps :: Monad m => e -> Trigger e m a -> m (Trigger e m a)
steps e (Wait m)   = snd <$> step' (-1) e m
steps e t@(Done _) = pure t

-- | Step a list of triggers on the given event.
--   Any triggers that terminate after processing this trigger are removed
--   from the list of triggers.
stepAll :: Monad m => e -> [Trigger e m a] -> m [Trigger e m a]
stepAll e = fmap (filter isWaiting) . mapM (step e)

-- | Execute the given trigger up until the first @await@ call.
step0 :: Monad m => Trigger e m a -> m (Trigger e m a)
step0 (Wait m)   = snd <$> step' 0 undefined m
step0 t@(Done _) = pure t

-- | Like 'stepAll', but only proceeds up until the first 'await' call.
stepAll0 :: Monad m => [Trigger e m a] -> m [Trigger e m a]
stepAll0 = fmap (filter isWaiting) . mapM step0

-- | Step the given triggered computation past at most @n@ calls to 'await'.
step' :: Monad m => Int -> e -> Triggered e m a -> m (Int, Trigger e m a)
step' steps e (Bind m f) = do
  res <- step' steps e m
  case res of
    (steps', Done x) -> step' steps' e (f x)
    (steps', Wait m') -> return (steps', Wait (m' >>= f))
step' steps _ (Lift m) = do
  x <- m
  return (steps, Done x)
step' steps _ (Pure x) = do
  pure (steps, Done x)
step' steps e Event = do
  pure (steps, Done e)
step' 0 e m@(Await p) = do
  return (0, Wait m)
step' steps e m@(Await p) = do
  match <- p e
  return $ if match then (steps-1, Done ()) else (steps, Wait m)

awaitM :: (e -> m Bool) -> Triggered e m ()
awaitM = Await

await :: Applicative m => (e -> Bool) -> Triggered e m ()
await f = awaitM (pure . f)

awaitVal :: (Applicative m, Eq e) => e -> Triggered e m ()
awaitVal = await . (==)
