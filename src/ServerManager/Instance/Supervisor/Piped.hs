{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
-- | Supervisor for servers communicating over stdin/out.
module ServerManager.Instance.Supervisor.Piped
  ( start
  ) where
import Prelude hiding (unwords)
import Control.Shell hiding (try, ask, lift)
import qualified Control.Shell as Shell
import Control.Shell.Concurrent as Shell hiding (await)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, pack, unpack, unwords)
import Data.Text.Encoding
import qualified Data.IntMap as M
import Data.Time
import Database.Selda
import Control.Monad.Triggered
import ServerManager.Instance
import ServerManager.Instance.Backup
import ServerManager.Instance.Command
import ServerManager.Instance.Manager.Type
import ServerManager.Instance.Supervisor
import ServerManager.Log
import ServerManager.Tables as Model hiding (Backup)
import ServerManager.ServerConfig

data ControllerState cfg resp = ControllerState
  { isStopping  :: Bool
  , triggers    :: [Trigger (LogMessage cfg) (Inst cfg resp) ()]
  , awaitEvent  :: Inst cfg resp (Event Command resp (LogMessage cfg))
  , putMessage  :: Text -> Inst cfg resp ()
  , crashes     :: [UTCTime]
  , watcherPid  :: ThreadId
  , instancePid :: ThreadId
  }

-- | Start the given instance, unless it is already running.
start :: ServerConfig cfg
      => Inst cfg resp (InstanceHandle Command resp (LogMessage cfg))
start = do
  executable <- getExecutable
  args <- getArgs
  workingDir <- getWorkingDirectory
  writeConfigs workingDir
  createPipedSupervisor workingDir executable args

hPutMessage :: MonadIO m => Handle -> Text -> m ()
hPutMessage h s = liftIO $ BS.hPutStrLn h (encodeUtf8 s)

logAndTrigger :: Typeable cfg
              => ControllerState cfg resp
              -> LogMessage cfg
              -> Inst cfg resp (ControllerState cfg resp)
logAndTrigger st msg = do
  trig <- asks (logTrigger . infoTriggers)
  logItem msg
  processTriggers msg $ st { triggers = trig : triggers st }

-- | Write all configuration files for the server to their appropriate
--   locations.
writeConfigs :: forall cfg resp. ServerConfig cfg => FilePath -> Inst cfg resp ()
writeConfigs root = do
  let svrCfgPath = root </> configPath (Proxy :: Proxy cfg)
  ps <- asks (Model.players . infoModel)
  writeWL <- asks infoWriteWhiteList
  writeBL <- asks infoWriteBanList
  writeOL <- asks infoWriteOpList
  players <- query $ select ps
  svrCfgText <- serializeConfig <$> getConfig
  liftIO $ do
    BS.writeFile svrCfgPath svrCfgText
    writeWL root [Model.playerName p | p <- players, Model.isWhiteListed p]
    writeBL root [Model.playerName p | p <- players, Model.isBanned p]
    writeOL root [Model.playerName p | p <- players, Model.isOp p]

-- | Creates an instance supervisor that communicates with its instance using
--   standard input/output.
createPipedSupervisor :: forall cfg resp.
                         ServerConfig cfg
                      => FilePath
                      -> FilePath
                      -> Model.ServerArgs
                      -> Inst cfg resp (InstanceHandle Command resp (LogMessage cfg))
createPipedSupervisor dir exe as = do
    info <- ask
    let parse = infoParser info
        logTrig  = logTrigger $ infoTriggers info
    (snk, src, hdl) <- createPipes
    trig <- step serverStartingMsg $ startTrigger (infoTriggers info)
    liftSh . forkIO . inDirectory dir $ do
      (i, o, instPid) <- fork2 $ run exe (map unpack $ args as)
      watchPid <- forkIO $ watcher parse snk o
      runInst info $ do
        let initialState = ControllerState
              { awaitEvent = nextEvent src
              , putMessage = hPutMessage i
              , isStopping = False
              , triggers = [trig]
              , crashes = []
              , watcherPid = watchPid
              , instancePid = instPid
              }
        st <- logAndTrigger initialState serverStartingMsg
        controller st
    return hdl
  where
    serverStartingMsg = item Info Manager "Starting server"

-- | Restart the current instance, forcefully terminating any previous
--   watcher and instance processes, and clearing the controller's state.
restart :: ServerConfig cfg => Inst cfg resp (ControllerState cfg resp)
restart = do
    exe <- getExecutable
    as <- map unpack . args <$> getArgs
    dir <- getWorkingDirectory
    writeConfigs dir
    (snk, src, hdl) <- createPipes
    notifyRestart hdl
    (i, o, instPid) <- liftSh $ inDirectory dir $ fork2 $ run exe as
    parse <- asks infoParser
    watchPid <- liftSh . forkIO $ watcher parse snk o
    trig <- step restartMsg =<< asks (restartTrigger . infoTriggers)
    pure $ ControllerState
      { awaitEvent = nextEvent src
      , putMessage = hPutMessage i
      , isStopping = False
      , triggers = [trig]
      , crashes = []
      , watcherPid = watchPid
      , instancePid = instPid
      }
  where
    restartMsg = item Info Manager "Restarting server"

-- | Watcher: responsible for monitoring the instance and passing any events
--   on to the controller.
watcher :: Typeable cfg
        => (BS.ByteString -> Maybe (LogMessage cfg))
        -> EventSink Command resp (LogMessage cfg)
        -> Handle
        -> Shell ()
watcher parse sink h = do
  emsg <- Shell.try $ do
    ln <- hGetByteLine h
    case parse ln of
      Just msg -> pure msg
      _        -> pure $ item Warn Manager $
        "Couldn't parse log line: " <> pack (BS.unpack ln)
  case emsg of
    Left e  -> reportDeath sink
    Right m -> postEvent sink m >> watcher parse sink h

-- | Controller: responsible for handling commands and events from the
--   watcher.
controller :: ServerConfig cfg
           => ControllerState cfg  resp
           -> Inst cfg resp ()
controller st = do
  evt <- awaitEvent st
  case evt of
    Event m              -> logAndTrigger st m >>= controller
    Command cmd response -> processCommand st cmd response >>= controller
    Stop _               -> initiateShutdown st >>= controller
    InstanceDied
      | isStopping st    -> gracefullyShutdown st
      | otherwise        -> abnormalShutdown st

-- | Tell the server to initiate a graceful shutdown.
initiateShutdown :: Typeable cfg
                 => ControllerState cfg resp
                 -> Inst cfg resp (ControllerState cfg resp)
initiateShutdown st = do
  setState Stopping
  stopTrig <- asks (stopTrigger . infoTriggers)
  addTriggers st [stopTrig]

-- | Server died while in state @Stopping@; everything is OK.
gracefullyShutdown :: Typeable cfg
                 => ControllerState cfg resp
                 -> Inst cfg resp ()
gracefullyShutdown st = do
  setState Stopped
  logAndTrigger st $ item Info Manager "Server shutdown completed"
  setGracefulShutdown True
  killInstance st
  removeFromInstanceManager
  dead <- asks infoDeathHandler
  dead True

-- | Server died while not in state @Stopping@.
abnormalShutdown :: ServerConfig cfg
                 => ControllerState cfg resp
                 -> Inst cfg resp ()
abnormalShutdown st = do
  setState Stopped
  setGracefulShutdown False
  st' <- logAndTrigger st $ item Severe Manager "Server died unexpectedly!"
  now <- liftIO getCurrentTime
  tryRestart now st'

-- | Restart the server, unless it's already been restarted too many times.
tryRestart :: ServerConfig cfg
           => UTCTime
           -> ControllerState cfg resp
           -> Inst cfg resp ()
tryRestart now st = do
    if shouldAttemptRestart
      then do
        restart >>= controller
      else do
        logAndTrigger st $
          item Severe Manager "Server died too many times; not restarting."
        removeFromInstanceManager
        asks infoDeathHandler >>= \death -> death False
  where
    crashWindowSecs = 60*15
    maxCrashesInWindow = 3

    crashWindow = [c | c <- now:crashes st, diffUTCTime now c < crashWindowSecs]
    shouldAttemptRestart = length crashWindow <= maxCrashesInWindow

-- | Forcibly kill an instance and its watcher.
killInstance :: Typeable cfg
             => ControllerState cfg resp
             -> Inst cfg resp ()
killInstance st = do
  liftSh $ killThread (instancePid st)
  liftSh $ killThread (watcherPid st)

-- | Process all waiting triggers.
processTriggers :: LogMessage cfg
                -> ControllerState cfg resp
                -> Inst cfg resp (ControllerState cfg resp)
processTriggers m st = do
  ts <- stepAll m (triggers st)
  return st { triggers = ts }

-- | Process a single command, then keep running the controller.
processCommand :: Typeable cfg
               => ControllerState cfg resp
               -> Command
               -> Maybe (Response resp)
               -> Inst cfg resp (ControllerState cfg resp)
processCommand st cmd resp = do
  t <- asks infoTriggers
  trigs <- case cmd of
    Ban player exp     -> pure [banTrigger t player exp]
    Unban player       -> pure [unbanTrigger t player]
    Op player          -> pure [opTrigger t player True]
    Unop player        -> pure [opTrigger t player False]
    WhiteList p exp    -> pure [whitelistTrigger t p exp]
    UnWhiteList p      -> pure [unwhitelistTrigger t p]
    Broadcast msg      -> pure [broadcastTrigger t msg]
    Backup             -> pure [backupTrigger t backupFiles]
    TextCommand cmd    -> putMessage st cmd >> pure []
  addTriggers st trigs

-- | Process a backup command.
processBackup :: Typeable cfg
              => ControllerState cfg resp
              -> Inst cfg resp (ControllerState cfg resp)
processBackup st = do
    backupTrig <- ($ backupFiles) <$> asks (backupTrigger . infoTriggers)
    logAndTrigger (st {triggers = backupTrig : triggers st})
                  backupStartedMsg
  where
    backupStartedMsg = item Info Manager "Backup started"
    backupFiles = undefined

-- | Add the given triggers to the given controller state, after executing
--   them up intil the first 'await' call.
addTriggers :: ControllerState cfg resp
            -> [Trigger (LogMessage cfg) (Inst cfg resp) ()]
            -> Inst cfg resp (ControllerState cfg resp)
addTriggers st ts = do
  ts' <- stepAll0 ts
  pure $ st { triggers = ts' ++ triggers st }

-- | Remove the instance from its instance manager's list of running instances.
--   Does not check if the instance is actually stopped.
removeFromInstanceManager :: Typeable cfg => Inst cfg resp ()
removeFromInstanceManager = do
  iid <- asks infoId
  modInsts <- asks infoModifyInstances
  modInsts $ M.delete (fromId iid)

-- | Mark the instance as either having shut down gracefully or abnormally.
setGracefulShutdown :: MonadInst cfg resp m => Bool -> m ()
setGracefulShutdown graceful = liftInst $ do
  iid <- asks infoId
  ins <- asks (instances . infoModel)
  update_ ins (#id `is` iid) $ \inst -> inst `with`
    [ #gracefulShutdown := true
    ]

-- | Notify the manager that the server has restarted, and now has the given
--   instance handle.
notifyRestart :: Typeable cfg
              => InstanceHandle Command resp (LogMessage cfg)
              -> Inst cfg resp ()
notifyRestart ih = do
    iid <- asks infoId
    modInsts <- asks infoModifyInstances
    modInsts $ M.update upd (fromId iid)
  where
    upd is = Just $ is
      { instanceState = Starting
      , instanceHandle = ih
      }
