-- | Tools for implementing instance supervisors.
--
--   An instance supervisor has two parts: the watcher and the controller.
--   The watcher is responsible for monitoring the instance process and
--   posting back any events to the controller. When the process dies, the
--   watcher should also die after sending the 'reportDeath' message.
--
--   The controller is responsible for sending commands to the instance
--   process, and for restarting the watcher (and instance process) if they
--   should die.
module ServerManager.Instance.Supervisor
  ( -- * Shared functionality
    createPipes

    -- * Implementing watchers
  , EventSink
  , postEvent, reportDeath

    -- * Implementing controllers
  , EventSource, Event (..), Response
  , nextEvent, respond

    -- * External callers
  , InstanceHandle
  , call, post, stop, stop'
  ) where
import Control.Monad.IO.Class
import Control.Concurrent

-- | A sink to which events can be written.
newtype EventSink req resp evt = EventSink
  { sinkChan :: Chan (Event req resp evt)
  }

-- | An event source which events can be read from.
newtype EventSource req resp evt = EventSource
  { sourceChan :: Chan (Event req resp evt)
  }

newtype InstanceHandle req resp evt = InstanceHandle
  { instanceHandle :: Chan (Event req resp evt)
  }

-- | A response sink, to which a response to a request can be written.
newtype Response a = Response (MVar a)

-- | Create a full set of sink, source and instance handle.
createPipes :: MonadIO m
            => m ( EventSink req resp evt
                 , EventSource req resp evt
                 , InstanceHandle req resp evt
                 )
createPipes = liftIO $ do
  c <- newChan
  return (EventSink c, EventSource c, InstanceHandle c)

-- | Make a single response to a command.
respond :: MonadIO m => Response a -> a -> m ()
respond (Response v) = liftIO . putMVar v

-- | Make a synchronous call to an instance controller.
call :: MonadIO m => InstanceHandle req resp evt -> req -> m resp
call (InstanceHandle hdl) req = liftIO $ do
  resp <- newEmptyMVar
  writeChan hdl (Command req (Just (Response resp)))
  takeMVar resp

-- | Make an asynchronous call to an instance controller.
post :: MonadIO m => InstanceHandle req resp evt -> req -> m ()
post (InstanceHandle hdl) req = liftIO $ writeChan hdl (Command req Nothing)

-- | Stop an instance, asynchronously.
stop :: MonadIO m => InstanceHandle req resp evt -> m ()
stop (InstanceHandle hdl) = liftIO $ writeChan hdl (Stop Nothing)

-- | Stop an instance, synchronously.
stop' :: MonadIO m => InstanceHandle req resp evt -> m resp
stop' (InstanceHandle hdl) = liftIO $ do
  resp <- newEmptyMVar
  writeChan hdl (Stop (Just (Response resp)))
  takeMVar resp

-- | Post an event to an event sink, asynchronously.
postEvent :: MonadIO m => EventSink req resp evt -> evt -> m ()
postEvent (EventSink c) = liftIO . writeChan c . Event

-- | Report the death of the instance process.
--   The supervisor must not post any additional messages after sending the
--   death message, but should instead terminate as soon as possible.
reportDeath :: MonadIO m => EventSink req resp evt -> m ()
reportDeath = liftIO . flip writeChan InstanceDied . sinkChan

-- | Wait for the next event to occur.
nextEvent :: MonadIO m => EventSource req resp evt -> m (Event req resp evt)
nextEvent = liftIO . readChan . sourceChan

-- | An event to be received by a controller.
data Event req resp evt
  = InstanceDied
  | Event !evt
  | Command !req (Maybe (Response resp))
  | Stop (Maybe (Response resp))
