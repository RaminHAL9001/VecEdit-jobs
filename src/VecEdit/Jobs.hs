-- | A job manager. A table of child processes is maintained, the
-- @STDIN@, @STDOUT@, and @STDERR@ of each process is captured in a
-- text buffer.
module VecEdit.Jobs
  ( -- ** The 'Manager' monad
    Manager, ManagerEnv, newManagerEnv, runManager,
    ManagerEnvState, bufferTable, workerTable, processTable,

    -- ** Text 'Buffer's
    Buffer, TextTags, newBuffer,
    bufferPrintAll, bufferList, bufferHandle, bufferFile, bufferFileInto, bufferShow,
    withBuffer, withBufferPath,

    -- ** 'Worker's (threads)
    Worker, WorkerStatus, startWork,
    workerPrintAll, workerList, workerGetStatus,

    -- ** 'Process'es and Jobs
    -- 
    -- These APIs deal with setting up jobs (POSIX processes), requesting the operating system run a
    -- child process, capturing the output and error streams of a child process, sending input to a
    -- child process input stream, and sending POSIX signals to the child process.
    
    -- *** Configuring a child process
    --
    -- You can configure a child process for synchronous or asynchronous execution. When
    -- asynchronous, there are several options for capturing the output and error streams of a child
    -- process. You can also define your own continuation for receiving raw strings from the output
    -- and error streams via an IO 'Handle'. Once a process is created and in the process table, it
    -- can be run as a job.
    ProcessConfig(..), ProcessProgramFile,
    PipeSenderConfig(..), PipeReceiverConfig(..),
    synchronous, asynchronous, asyncCapture, asyncCaptureNewBuffer,

    -- *** Custom 'PipeReceiverConfigFunction' configurations
    bufferReceiveLines, bufferReceiveLinesPut,
    
    -- *** Running a 'Process' as a job
    Process, newProcess, runProcess,
    processPrintAll, processList,
    processSend, processGetState, processWait, processCancel,
    processGetCaptureBuffer,
    GetProcessBuffer(..), ProcessLog(..), processGetLog,

    -- ** Inspecting global state
    GlobalTableSearchIndex(..),
  ) where

import VecEdit.Types
  ( VectorSize, RelativeDirection(..), TextRange(..), LineIndex(..),
    EditTextError(..),
  )

import VecEdit.Vector.Editor ( ralign6 )
import VecEdit.Vector.Editor.GapBuffer (newGapBufferState)
import VecEdit.Print.DisplayInfo (DisplayInfo(..), displayInfoShow, showAsText)
import VecEdit.Text.String
  ( TextLine, ReadLines, HaltReadLines, StringData,
    fromStringData, bytesFromString, hReadLines, hReadLineBufferSize,
    convertString, theTextLineData,
  )
import VecEdit.Text.Editor
  ( EditText, EditTextState, loadHandleEditText, loadHandleEditText,
    newEditTextState, runEditText, pushLine, foldLinesInRange,
    maxLineIndex, validateBounds, mapRangeFreeze,
  )
import qualified VecEdit.Table as Table
import VecEdit.Table (Table)

import Control.Concurrent (ThreadId, forkIO, yield)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar, withMVar)
import Control.Exception (SomeException, catch)
import Control.Lens (Lens', lens, use, (^.), (.~), (%=), (.=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask, ReaderT(..))
import Control.Monad.State (StateT(..), MonadState(..), runStateT)
import Control.Monad.Trans (lift)

import qualified Data.ByteString as Bytes
import Data.Function (on)
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Vector as Vec
import Data.Vector (Vector)

import System.IO (Handle, IOMode(..), openFile, hClose, hFlush)
import qualified System.Process as Exec
import System.Process
       ( ProcessHandle,
         waitForProcess, getProcessExitCode, interruptProcessGroupOf
       )
import System.Exit (ExitCode(..))

----------------------------------------------------------------------------------------------------

newtype Buffer = Buffer (MVar BufferState)
  deriving (Eq)

type TextTags = ()

-- | This is an element of a 'ManagerEnv' which contains metadata for manipulating the buffer in
-- GHCI. This is also the content accessible by a 'Buffer' handle.
data BufferState
  = BufferState
    { theBufStateFilePath :: !Strict.Text
    , theBufStateLockedBy :: !(Maybe (Table.Row Worker))
    , theBufStateBuffer   :: !(EditTextState TextTags)
    }

instance DisplayInfo BufferState where
  displayInfo putStr
    (BufferState
     {theBufStateFilePath=path
     ,theBufStateLockedBy=locked
     }) = do
      putStr path
      maybe (pure ()) (\ a -> putStr " " >> displayInfo putStr a) locked

instance DisplayInfo Buffer where
  displayInfo putStr (Buffer mvar) =
    readMVar mvar >>=
    displayInfo putStr

bufStateBuffer :: Lens' BufferState (EditTextState TextTags)
bufStateBuffer = lens theBufStateBuffer $ \ a b -> a{ theBufStateBuffer = b }

bufStateFilePath :: Lens' BufferState Strict.Text
bufStateFilePath = lens theBufStateFilePath $ \ a b -> a{ theBufStateFilePath = b }

----------------------------------------------------------------------------------------------------

-- | This is a thread that can perform work without freezing the controlling thread which runs the
-- REPL.
data Worker
  = Worker
    { theWorkerId :: !ThreadId
    , theWorkerStatus :: !(MVar WorkerStatus)
    }

-- | The "status" of a worker is the state of operation the 'Worker' is in at any given time.
data WorkerStatus
  = Ready
  | Working
  | Success
  | WorkerError !EditTextError
  | WorkerFailed !SomeException
  deriving Show

instance Eq  Worker where { (==) = (==) `on` theWorkerId; }
instance Ord Worker where { compare = compare `on` theWorkerId; }

instance DisplayInfo Worker where
  displayInfo putStr (Worker{theWorkerId=thid,theWorkerStatus=mvar}) = do
    stat <- readMVar mvar
    displayInfoShow putStr thid
    putStr " "
    displayInfoShow putStr stat

-- | Get the state of operation of a 'Worker'.
workerGetStatus :: MonadIO io => Worker -> io WorkerStatus
workerGetStatus = liftIO . readMVar . theWorkerStatus

----------------------------------------------------------------------------------------------------

-- | An entry in the global process table, it can be used to launch and relaunch processes, retrieve
-- the process exit code, and retrieve any output from the process that was captured in a buffer.
newtype Process = Process (MVar ProcessConfig)

data ProcessConfig
  = ProcessConfig
    { theProcConfigCommand :: !ProcessProgramFile
    , theProcConfigArgs :: !(Vector Strict.Text)
    , theProcConfigInPipe :: !PipeSenderConfig
    , theProcConfigOutPipe :: !PipeReceiverConfig
    , theProcConfigErrPipe :: !PipeReceiverConfig
    , theProcConfigState :: !ProcessState
    }

-- | When a function takes an argument of this type, the given 'Strict.Text' string must be a path
-- to an executable program file somewhere in the file system.
type ProcessProgramFile = Strict.Text

-- | This data type is part of a 'ProcessConfig', and is used to configure how the STDIN stream of a
-- child process will be used.
data PipeSenderConfig
  = PipeSenderConfigClose -- ^ Close the sender file handle as soon as the process is craeted.
  | PipeSenderConfigInherit -- ^ Use the sender file handle for the current process
  | PipeSenderConfigPipe -- ^ Create a pipe for sending to the child process
  | PipeSenderConfigRedirect !Handle -- ^ Use an already open pipe as the input for the child process

-- | This data type is a part of a 'ProcessState' for a 'ProcRunning' (running process).  It
-- contains information necessary to enact the 'PipeReceiveConfig' semantics for both STDOUT and
-- STDERR streams.
newtype PipeSender = PipeSender (Maybe Handle)

-- | This data type is part of a 'ProcessConfig' controlling how the STDOUT and STDERR streams of a
-- process will be used.
data PipeReceiverConfig
  = PipeReceiverConfigClose -- ^ Close the stream after forking the child process.
  | PipeReceiverConfigInherit -- ^ Inherit the pipe from the current process
  | PipeReceiverConfigNewBuffer -- ^ Launch a thread to capture the 'Handle' into a newly created 'Buffer'.
  | PipeReceiverConfigBuffer
    { pipeControlBuffer :: !(Table.Row Buffer)
    }  -- ^ Launch a thread to capture the 'Handle' into a 'Buffer'.
  | PipeReceiverConfigHandle
    { pipeControlHandle :: !Handle
    } -- ^ Redirect to an already-open 'Handle'
  | PipeReceiverConfigFile
    { pipeControlFile :: !Strict.Text
    , pipeControlAppend :: !Bool
      -- ^ 'True' -> 'AppendMode', 'False' -> 'WriteMode' (truncate)
    } -- ^ Redirect to a 'Handle' by opening the given file path.
  | PipeReceiverConfigNewBufferWithFile
    { pipeControlFile :: !Strict.Text
    , pipeControlAppend :: !Bool
      -- ^ 'True' -> 'AppendMode', 'False' -> 'WriteMode' (truncate)
    } -- ^ Launch a thread to capture the 'Handle' into a 'Buffer', but also write captured output
      -- to a file.
  | PipeReceiverConfigFunction
    { pipeControlDescription :: !Strict.Text
    , pipeControlFunction :: !(Handle -> IO (Either EditTextError ()))
    } -- ^ Launch a thread to read or write the 'Handle'. Provide a descriptive string, otherwise
     -- what this function actually does with the 'Handle' is completely opaque. For all other
     -- stream receiver/sender mechanisms that are not handled by the other constructors of this
     -- data type, you can define your own with this constructor.

-- | This data type is a part of a 'ProcessState' for a 'ProcRunning' (running process). It contains
-- information necessary to enact the 'PipeReceiverConfig' semantics for both STDOUT and STDERR
-- streams.
data PipeReceiver
  = PipeReceiverNothing
  | PipeReceiverInherit
  | PipeReceiverHandle{ pipeConnectHandle :: !Handle }
    -- ^ Redirect to an already-open 'Handle'
  | PipeReceiverFile
    { pipeConnectFile :: !Strict.Text
    , pipeConnectFileAppend :: !Bool
      -- ^ Only relevant for receiver pipes. This is ignored for sender pipes.
    , pipeConnectHandle :: !Handle
    }
  | PipeReceiverBuffer
    { pipeConnectHandle :: !Handle
    , pipeConnectBuffer :: !(Table.Row Buffer)
    , pipeConnectThread :: !ThreadId
    , pipeConnectWaitClose :: !(MVar (Either EditTextError ()))
    }
  | PipeReceiverNewBufferWithFile
    { pipeConnectHandle :: !Handle
    , pipeConnectFileHandle :: !Handle
    , pipeConnectFile :: !Strict.Text
    , pipeConnectFileAppend :: !Bool
      -- ^ Only relevant for receiver pipes. This is ignored for sender pipes.
    , pipeConnectBuffer :: !(Table.Row Buffer)
    , pipeConnectThread :: !ThreadId
    , pipeConnectWaitClose :: !(MVar (Either EditTextError ()))
    }
  | PipeReceiverFunction
    { pipeConnectDescription :: !Strict.Text
    , pipeConnectHandle :: !Handle
    , pipeConnectThread :: !ThreadId
    , pipeConnectWaitClose :: !(MVar (Either EditTextError ()))
    } -- ^ For all other stream receiver/sender mechanisms that are not handled by the other
      -- constructors of this data type, you can define your own with this constructor.

-- | Describes what state a 'Process' is in.
data ProcessState
  = ProcPending -- ^ Process is configured but hasn't been run yet.
  | ProcRunning
    { theProcStateProcHandle :: !ProcessHandle
    , theProcStatePid :: !Exec.Pid
    , theProcWaitThread :: !ThreadId
    , theProcStartTime :: !UTCTime
    , theProcInPipe :: !PipeSender
    , theProcOutPipe :: !PipeReceiver
    , theProcErrPipe :: !PipeReceiver
    } -- ^ Process is running with this given handle.
  | ProcHalted
    { theProcStateExitCode :: !ExitCode
    , theProcStartTime :: !UTCTime
    , theProcEndTime :: !UTCTime
    , theProcInPipe :: !PipeSender
    , theProcOutPipe :: !PipeReceiver
    , theProcErrPipe :: !PipeReceiver
    } -- ^ Process halted with this exit code.

-- | This data structure represents a portion of a process log. It is returned by 'processGetSTDOUT'
-- and 'processGetSTDERR'. Values of this type are only for reporting, so there is a 'ToJSON'
-- instance but no 'FromJSON' instance.
data ProcessLog
  = ProcessNotFound
    { theProcessLogHandle :: !(Table.Row Process)
    }
  | ProcessTextError !EditTextError
  | ProcessLog
    { theProcessLogHandle :: !(Table.Row Process) -- ^ the 'Table.RowId' of the process
    , theProcessLogRange :: !(TextRange LineIndex) -- ^ The range of lines retrieved
    , theProcessLog :: !(Vector (TextLine TextTags))
    }

instance Show ProcessConfig where
  show cfg =
    "(ProcessConfig " <> show (theProcConfigCommand cfg) <>
    " " <> show (show <$> Vec.toList (theProcConfigArgs cfg)) <>
    " :stdout " <> show (theProcConfigOutPipe cfg) <>
    " :stderr " <> show (theProcConfigErrPipe cfg) <>
    " :state " <> show (theProcConfigState cfg) <>
    ")"

instance Show PipeReceiverConfig where
  show = \ case
    PipeReceiverConfigClose -> "(close)"
    PipeReceiverConfigInherit -> "(inherit)"
    PipeReceiverConfigNewBuffer -> "(capture-new-buffer)"
    PipeReceiverConfigBuffer buf -> "(capture-buffer " <> show (Table.theRowLabel buf) <> ")"
    PipeReceiverConfigHandle _ -> "(file-handle)"
    PipeReceiverConfigFile file _ -> "(file " <> show file <> ")"
    PipeReceiverConfigNewBufferWithFile file append ->
      "(capture-buffer " <> show file <> (if append then ":append-mode" else "") <> ")"
    PipeReceiverConfigFunction desc _ -> "(capturing " <> show desc <> ")"

instance Show PipeReceiver where
  show = \ case
    PipeReceiverNothing -> "(none)"
    PipeReceiverInherit -> "(inherited)"
    PipeReceiverHandle _ -> "(redirect)"
    PipeReceiverFile file append _ ->
      "(redirect-file " <> show file <>
      (if append then " :append-mode" else "") <> ")"
    PipeReceiverBuffer _ buf thid _ ->
      "(capture-buffer " <> show thid <> " " <> show (Table.theRowLabel buf) <> ")"
    PipeReceiverNewBufferWithFile _ _ file append buf thid _ ->
      "(capture-buffer-log-file " <> show file <>
      (if append then " :append-mode" else "") <>
      show thid <> " " <> show (Table.theRowLabel buf) <> ")"
    PipeReceiverFunction desc _ thid _ ->
      "(captured " <> show desc <> " :" <> show thid <> ")"

instance Show ProcessState where
  show = \ case
    ProcPending -> "ProcPending"
    ProcRunning{theProcStatePid=pid} -> "ProcRunning " <> show pid
    ProcHalted{theProcStateExitCode=exid} -> "ProcHalted " <> show exid

instance DisplayInfo ProcessConfig where { displayInfo = displayInfoShow; }


instance DisplayInfo Process where
  displayInfo putStr (Process mvar) = readMVar mvar >>= displayInfo putStr

instance DisplayInfo ProcessState where
  displayInfo putStr = \ case
    ProcPending -> putStr "(pending)\n"
    ProcRunning{theProcStatePid=pid} ->
      mapM_ putStr ["(running, pid=", showAsText pid, ")\n"]
    ProcHalted{theProcStateExitCode=exid} ->
      mapM_ putStr ["(halted, ", showAsText exid, ")\n"]

instance DisplayInfo ProcessLog where
  displayInfo putStr = \ case
    ProcessNotFound{ theProcessLogHandle=row } -> do
      putStr "process not found: "
      displayInfo putStr row
      putStr "\n"
    ProcessTextError err -> do
      displayInfo putStr err
      putStr "\n"
    ProcessLog{ theProcessLogHandle=row, theProcessLogRange=range, theProcessLog=vec } -> do
      putStr "Process id="
      displayInfo putStr row
      putStr "\n"
      forM_ (zip (iterate (+ 1) (theTextRangeStart range)) (Vec.toList vec)) $ \ (i, line) ->
        putStr $ Strict.pack $ "  " <> ralign6 i <> ": " <> show line <> "\n"

procConfigState :: Lens' ProcessConfig ProcessState
procConfigState = lens theProcConfigState $ \ a b -> a{ theProcConfigState = b }

-- | Not for export. This function is used to translate a 'PipeSenderConfig' into a 'Exec.StdStream'
-- for initializing a 'Exec.CreateProcess' data structure.
pipeSenderStdStream :: PipeSenderConfig -> Exec.StdStream
pipeSenderStdStream = \ case
  PipeSenderConfigClose -> Exec.NoStream
  PipeSenderConfigInherit -> Exec.Inherit
  PipeSenderConfigPipe -> Exec.CreatePipe
  PipeSenderConfigRedirect handle -> Exec.UseHandle handle

-- | Not for export. This function is used to translate a 'PipeSenderConfig' into a 'PipeSender' for
-- a 'ProcessState'.
pipeSender :: Maybe Handle -> PipeSenderConfig -> PipeSender
pipeSender procHandle = \ case
  PipeSenderConfigClose -> PipeSender Nothing
  PipeSenderConfigInherit -> PipeSender Nothing
  PipeSenderConfigPipe -> PipeSender procHandle
  PipeSenderConfigRedirect{} -> PipeSender Nothing
    -- 'PipeSenderRedirect' returns Nothing because the handle being read by the child process is not
    -- also supposed to be read by the parent process. You still have access to this handle from the
    -- original 'PipeSenderConfig' but the result of this function should be a writeable handle that
    -- can be used to write to the child process, not a handle that is to be read from. For this
    -- reason, 'PipeSenderPipe' is the only constructor that returns the given 'procHandle', this is
    -- the only constructor that returns a writable pipe.

-- | If there are any open file 'Handle's in a given 'PipeReceiver', close them now with 'hClose'.
closePipeReceiver :: PipeReceiver -> IO ()
closePipeReceiver = \ case
  PipeReceiverHandle h -> hClose h
  PipeReceiverFile _ _ h -> hClose h
  PipeReceiverBuffer h _ _ _ -> hClose h
  PipeReceiverNewBufferWithFile h fh _ _ _ _ _ -> hClose h >> hClose fh
  PipeReceiverFunction _ h _ _ -> hClose h
  _ -> pure ()

-- | Close the 'Handle' for the 'PipeSender' if it exists and is still open.
closePipeSender :: PipeSender -> IO ()
closePipeSender (PipeSender h) = maybe (pure ()) hClose h

-- | This function is intended to be used to define a continuation for the
-- 'PipeReceiverConfigfunction' constructor of the 'PipeReceiverConfig' data type. It constructs a
-- function that reads line-break delimited strings from a 'Handle' and loops over it with a given
-- continuation function. This is a more general function used that takes an arbitrary @fold@ state,
-- and is used to define the 'bufferReceiveLines' and 'bufferReceiverLinesPut' functions which do
-- something more specific with the 'TextLine's generated.
receiveLineHandler
  :: fold
  -> (HaltReadLines fold void -> TextLine tags -> ReadLines fold ())
  -> Handle
  -> IO (Either EditTextError (), fold)
receiveLineHandler fold useLine procHandle =
  newGapBufferState Nothing hReadLineBufferSize >>= \ lineBuffer ->
  hReadLines lineBuffer procHandle useLine fold <*
  hClose procHandle

-- | This function is intended to be used to define a continuation for the
-- 'PipeReceiverConfigFunction' constructor of the 'PipeReceiverConfig' data type. This function is
-- defined by passing a continuation to 'receiveLineHandler' which simply stores all received lines
-- into a given 'Buffer', rather than passing these lines to a handler function.
bufferReceiveLines :: Table.Row Buffer -> Handle -> IO (Either EditTextError ())
bufferReceiveLines = fmap (fst <$>) . bufferReceiveLinesPut () (\ _ _ -> pure ())

-- | This function is intended to be used to define a continuation for the
-- 'PipeReceiverConfigfunction' constructor of the 'PipeReceiverConfig' data type. Like
-- 'bufferReceiveLines', it is defined by passing a continuation to 'receiveLineHandler' that stores
-- all received lines into a given 'Buffer', but also allows you to pass your own 'ReadLines'
-- continuation which is also called for each line of text, allowing the 'TextLine' to be both
-- buffered into the given 'Buffer' and also folded (e.g. logged to a file, or to STDOUT) by your
-- own continuation function.
bufferReceiveLinesPut
  :: fold
  -> (HaltReadLines fold void -> TextLine TextTags -> ReadLines fold ())
  -> Table.Row Buffer
  -> Handle
  -> IO (Either EditTextError (), fold)
bufferReceiveLinesPut fold put row =
  receiveLineHandler fold
  (\ halt line -> do
    liftIO $ withBuffer row $ pushLine Before line
    put halt line
  )

-- | Inspect a 'PipeReceiverConfig' and return an 'Exec.StdStream' value that can be used to
-- construct a 'PipeReceiver', either with 'connectPipeReceiver' or 'connectPipeSender'. This
-- function attempts to open a file if a 'PipeReceiverConfigFile' or
-- 'PipeReceiverConfigNewBufferWithFile' is given.
pipeReceiverConfigStdStream :: PipeReceiverConfig -> IO Exec.StdStream
pipeReceiverConfigStdStream = \ case
  PipeReceiverConfigClose -> pure Exec.NoStream
  PipeReceiverConfigInherit -> pure Exec.Inherit
  PipeReceiverConfigNewBuffer -> pure Exec.CreatePipe
  PipeReceiverConfigBuffer{} -> pure Exec.CreatePipe
  PipeReceiverConfigFile path append ->
    Exec.UseHandle <$>
    openFile (Strict.unpack path) (if append then AppendMode else WriteMode)
  PipeReceiverConfigNewBufferWithFile{} -> pure Exec.CreatePipe
  PipeReceiverConfigFunction{} -> pure Exec.CreatePipe
  PipeReceiverConfigHandle{ pipeControlHandle=h } -> pure $ Exec.UseHandle h

-- | Evaluate a 'PipeReceiverConfig' to a readable 'Handle', from either a STDOUT or STDERR stream of
-- a child 'Process'.
pipeReceiver
  :: Maybe Handle
  -> ProcessConfig
  -> PipeReceiverConfig
  -> StateT ManagerEnvState IO PipeReceiver
pipeReceiver procHandle cfg pipeControl =
  let requirePipe f = maybe (pure PipeReceiverInherit) f procHandle
      makeHandlerThread procHandle f = liftIO $ do
        waitMVar <- newEmptyMVar
        thid <- forkIO $ f procHandle >>= putMVar waitMVar -- brackets here?
        pure (thid, waitMVar)
      makeConnectBuffer put row procHandle =
        makeHandlerThread procHandle $
        (<* (hClose procHandle)) .
        fmap fst . bufferReceiveLinesPut () put row
  in case pipeControl of
    PipeReceiverConfigClose -> pure PipeReceiverNothing
    PipeReceiverConfigInherit -> pure PipeReceiverInherit
    PipeReceiverConfigHandle h -> pure $ PipeReceiverHandle{ pipeConnectHandle = h }
    PipeReceiverConfigFile path append -> requirePipe $ pure . PipeReceiverFile path append
    PipeReceiverConfigBuffer row -> requirePipe $ \ procHandle -> do
      (thid, wait) <- makeConnectBuffer (\ _ _ -> pure ()) row procHandle
      pure PipeReceiverBuffer
        { pipeConnectHandle = procHandle
        , pipeConnectBuffer = row
        , pipeConnectThread = thid
        , pipeConnectWaitClose = wait
        }
    PipeReceiverConfigNewBuffer -> requirePipe $ \ procHandle -> do
      buf <- internalNewBuffer (showAsText cfg) 63
      (thid, wait) <- makeConnectBuffer (\ _ _ -> pure ()) buf procHandle
      pure PipeReceiverBuffer
        { pipeConnectHandle = procHandle
        , pipeConnectBuffer = buf
        , pipeConnectThread = thid
        , pipeConnectWaitClose = wait
        }
    PipeReceiverConfigNewBufferWithFile path append -> requirePipe $ \ procHandle -> do
      buf <- internalNewBuffer (showAsText cfg) 63
      logHandle <- liftIO $
        openFile (Strict.unpack path) (if append then AppendMode else WriteMode)
      (this, wait) <- makeConnectBuffer
        (const $ liftIO . Bytes.hPutStr logHandle . bytesFromString)
        buf procHandle
      liftIO $ hClose logHandle
      pure PipeReceiverNewBufferWithFile
        { pipeConnectHandle = procHandle
        , pipeConnectFileHandle = logHandle
        , pipeConnectFile = path
        , pipeConnectFileAppend = append
        , pipeConnectBuffer = buf
        , pipeConnectThread = this
        , pipeConnectWaitClose = wait
        }
    PipeReceiverConfigFunction desc f -> requirePipe $ \ procHandle -> do
      (this, wait) <- makeHandlerThread procHandle f
      pure PipeReceiverFunction
        { pipeConnectDescription = desc
        , pipeConnectHandle = procHandle
        , pipeConnectThread = this
        , pipeConnectWaitClose = wait
        }

-- | Create a label from a 'ProcessConfig' which is used as the label in the 'processTable' of the
-- 'ManagerEnv'.
processCreateLabel :: ProcessConfig -> Strict.Text
processCreateLabel cfg =
  Strict.unwords $
  theProcConfigCommand cfg :
  Vec.toList (theProcConfigArgs cfg)

-- | Construct an initial 'Exec.CreateProcess' from a 'ProcessConfig'.
processConfigCreate :: ProcessConfig -> IO Exec.CreateProcess
processConfigCreate cfg = do
  outHandle <- pipeReceiverConfigStdStream $ theProcConfigOutPipe cfg
  errHandle <- pipeReceiverConfigStdStream $ theProcConfigErrPipe cfg
  return $
    ( Exec.proc
      (Strict.unpack $ theProcConfigCommand cfg)
      (Strict.unpack <$> Vec.toList (theProcConfigArgs cfg))
    ) { Exec.std_in = pipeSenderStdStream $ theProcConfigInPipe cfg
      , Exec.std_out = outHandle
      , Exec.std_err = errHandle
      }

-- | Not for export. Takes a 'Process' for which 'theProcConfigState' is 'ProcPending', and calls
-- the 'Exec.createProcess' function to ask the operating system to run the child process.
runProcess0 :: Process -> Manager (Either Strict.Text Exec.ProcessHandle)
runProcess0 (Process mvar) = do
  mgrst <- Manager (lift get)
  (result, mgrst) <- liftIO $ modifyMVar mvar $ \ cfg -> case theProcConfigState cfg of
    ProcRunning{} -> pure
      ( cfg
      , ( Left $ Strict.pack $ "(proc \"already running\" " <> show cfg <> ")"
        , mgrst
        )
      )
    _ -> do
      t0 <- getCurrentTime
      (pin, pout, perr, phandle) <- processConfigCreate cfg >>= Exec.createProcess
      pid <- Exec.getPid phandle
      let inPipe = pipeSender pin $ theProcConfigInPipe cfg
      ((outPipe, errPipe), mgrst) <- flip runStateT mgrst $
        (,) <$>
        pipeReceiver pout cfg (theProcConfigOutPipe cfg) <*>
        pipeReceiver perr cfg (theProcConfigOutPipe cfg)
      let waitForEnd = do
            exitCode <- waitForProcess phandle
            closePipeSender inPipe
            closePipeReceiver outPipe
            closePipeReceiver errPipe
            pure exitCode
      case pid of
        Nothing -> do
          exitCode <- waitForEnd
          t1 <- getCurrentTime
          pure
            ( procConfigState .~
              ProcHalted
              { theProcStateExitCode = exitCode
              , theProcStartTime = t0
              , theProcEndTime = t1
              , theProcInPipe = inPipe
              , theProcOutPipe = outPipe
              , theProcErrPipe = errPipe
              } $ cfg
            , ( Left $ Strict.pack $ "(proc \"failed\" :exit-code " <> show exitCode <> ")"
              , mgrst
              )
            )
        Just pid -> do
          waitThread <- forkIO $ do
            exitCode <- waitForEnd
            t1 <- getCurrentTime
            modifyMVar_ mvar $ pure .
              ( procConfigState .~
                ProcHalted
                { theProcStateExitCode = exitCode
                , theProcStartTime = t0
                , theProcEndTime = t1
                , theProcInPipe = inPipe
                , theProcOutPipe = outPipe
                , theProcErrPipe = errPipe
                }
              )
          pure
            ( procConfigState .~
              ProcRunning
              { theProcStateProcHandle = phandle
              , theProcStatePid = pid
              , theProcWaitThread = waitThread
              , theProcStartTime = t0
              , theProcInPipe = inPipe
              , theProcOutPipe = outPipe
              , theProcErrPipe = errPipe
              } $ cfg
            , ( Right phandle
              , mgrst
              )
            )
  Manager (lift $ put mgrst)
  pure result

-- | Run a particular 'Process' that has been created by 'newProcess' function. If the 'Process' has
-- been run before by 'runProcess' and has exited, the process will be re-run and the 'Table.Row'
-- entry will be reused so that it will have the same row ID and label as before. If the 'Process'
-- is currently running, this function returns a 'Left' error message.
runProcess :: Table.Row Process -> Manager (Either Strict.Text Exec.ProcessHandle)
runProcess = runProcess0 . Table.theRowObject

-- | Create, but do not execute, a new child process specification according to a
-- 'ProcessConfig'. The 'ProcessConfig' can be created with 'asynchronous', 'asyncCapture',
-- 'synchronous', or by setting up your own 'ProcessConfig'. Once the @'Table.Row' 'Process'@ is
-- returned, you can request the operating system run the child process using 'runProcess'.
newProcess :: ProcessConfig -> Manager (Table.Row Process)
newProcess cfg =
  editManagerEnvTable processTable $
  liftIO (newMVar cfg) >>= Table.insert (processCreateLabel cfg) . Process

-- | Setup a new synchronous child 'Process', which will use the same 'stdin', 'stdout', and
-- 'stderr' stream as the current process, and blocking the current process until the child process
-- completes. So 'theProcConfigInPipe' is set to 'PipeSenderConfigInherit', and both
-- 'theProcConfigOutPipe' and 'theProcConfigErrPipe' are set to 'PipeReceiverConfigInherit'.
synchronous :: ProcessProgramFile -> [Strict.Text] -> ProcessConfig
synchronous cmd args = ProcessConfig
  { theProcConfigCommand = cmd
  , theProcConfigArgs = Vec.fromList args
  , theProcConfigInPipe = PipeSenderConfigInherit
  , theProcConfigOutPipe = PipeReceiverConfigInherit
  , theProcConfigErrPipe = PipeReceiverConfigInherit
  , theProcConfigState = ProcPending
  }

-- | Setup a new asynchronous child 'Process', which will create two new buffers for both the
-- 'stdout' and 'stderr' streams, and create a pipe for the 'stdin' stream to which input can be
-- written with 'processSend'. So 'theProcConfigInPipe' is set to 'PipeSenderConfigPipe', and both
-- 'theProcConfigOutPipe' and 'theProcConfigErrPipe' are set to 'PipeReceiverConfigNewBuffer'.
asynchronous :: ProcessProgramFile -> [Strict.Text] -> ProcessConfig
asynchronous cmd args =
  (synchronous cmd args)
  { theProcConfigInPipe = PipeSenderConfigPipe
  , theProcConfigOutPipe = PipeReceiverConfigNewBuffer
  , theProcConfigErrPipe = PipeReceiverConfigNewBuffer
  }

-- | Setup a new asynchronous child 'Process', which will capture both 'stdout' and 'stderr' streams
-- into a given 'Buffer', and create a pipe for the 'stdin' stream to which input can be written
-- with 'processSend'. So 'theProcConfigInPipe' is set to 'PipeSenderConfigPipe', and both
-- 'theProcConfigOutPipe' and 'theProcConfigErrPipe' are set to 'PipeReceiverConfigBuffer' with the
-- same 'Buffer' for both.
asyncCapture :: ProcessProgramFile -> [Strict.Text] -> Table.Row Buffer -> ProcessConfig
asyncCapture cmd args buffer =
  (asynchronous cmd args)
  { theProcConfigOutPipe = PipeReceiverConfigBuffer buffer
  , theProcConfigErrPipe = PipeReceiverConfigBuffer buffer
  }

-- | Like 'asyncCapture', but automatically generates a new 'Buffer' for the 'ProcessConfig'. Also
-- note that this needs to be evaluated in a 'Manager' function context, so that a new 'Buffer' can
-- be created.
asyncCaptureNewBuffer :: ProcessProgramFile -> [Strict.Text] -> Manager (Table.Row Buffer, ProcessConfig)
asyncCaptureNewBuffer cmd args = do
  buf <- newBuffer ("ASYNC: " <> Strict.unwords (cmd : args)) 63
  pure (buf, asyncCapture cmd args buf)

-- | Get the 'ProcessState' for a 'Process'.
processGetState :: MonadIO io => Table.Row Process -> io ProcessState
processGetState row = liftIO $
  let (Process mvar) = Table.theRowObject row in
  theProcConfigState <$> readMVar mvar

-- | Not for export. Operate on a process only if 'theProcConfigState' is 'ProcRunning', throw an
-- error otherwise.
processRequireRunning
  :: MonadIO io
  => Table.Row Process
  -> (ProcessState -> IO (Either Strict.Text a))
  -> io (Either Strict.Text a)
processRequireRunning row f = liftIO $
  let (Process mvar) = Table.theRowObject row in
  withMVar mvar $ \ cfg -> case theProcConfigState cfg of
    st@(ProcRunning{}) -> f st
    _ -> pure $ Left $ Strict.pack $ show (Table.theRowLabel row) <> " process not running"

-- | If the 'Process' was created with the 'std_in' field of the 'CreateProcess' spec set to
-- 'CreatePipe', a 'Handle' is available for sending input to the Process (unless the 'Process' has
-- recently closed this handle). This function allows you to send a string of input to the
-- 'Process'.
processSend :: MonadIO io => Table.Row Process -> StringData -> io (Either Strict.Text ())
processSend row str =
  processRequireRunning row $ \ runningProc ->
  let (PipeSender inHandle) = theProcInPipe runningProc in
  case inHandle of
    Nothing -> pure $ Left $ Table.theRowLabel row <> " process created without input pipe"
    Just procHandle ->
      Right <$>
      case fromStringData str of
        Nothing -> pure ()
        Just txt -> Strict.hPutStr procHandle txt >> hFlush procHandle

-- | Send a signal to the given 'Process' handle forcing it to halt. This uses
-- 'interruptProcessGroupOf', which sends the POSIX signal @SIGINT@ ("interrupt") to the operating
-- system process.
processCancel :: MonadIO io => Table.Row Process -> io (Either Strict.Text ExitCode)
processCancel row = processRequireRunning row $ \ runningProc ->
  let procHandle = theProcStateProcHandle runningProc in
  interruptProcessGroupOf procHandle >>
  yield >>
  (\ case
    Nothing -> Left $ Strict.pack $ show (Table.theRowLabel row) <> " process not running"
    Just procHandle -> Right procHandle
  ) <$>
  getProcessExitCode procHandle

-- | Synchrnous wait on 'Process' completion, that is, freeze the current thread until the 'Process'
-- completes, then return the 'ProcessState' with the 'ExitCode'. Returns 'Nothing' if the 'Process'
-- has not started yet.
processWait :: MonadIO io => Table.Row Process -> io (Either Strict.Text ExitCode)
processWait row = processRequireRunning row $ fmap Right . waitForProcess . theProcStateProcHandle

data GetProcessBuffer = ProcessSTDOUT | ProcessSTDERR
  deriving (Eq, Ord, Show)

evalGetProcessBuffer :: GetProcessBuffer -> ProcessState -> Maybe PipeReceiver
evalGetProcessBuffer which st =
  ( case which of
      ProcessSTDOUT -> theProcOutPipe
      ProcessSTDERR -> theProcErrPipe
  ) <$> case st of
    ProcPending -> Nothing
    st -> Just st

-- | Obtain a handle for a capture buffer for a particular 'Process', if there are any. The
-- continuation passed as an argument is evaluated while the 'Process' table is locked to prevent
-- changes to the 'Buffer' that could occur while inspecting it. Return the 'Buffer' handle
-- immediately if you do not care to lock the 'Process' out of editing it's own capture 'Buffer'.
processGetCaptureBuffer
  :: MonadIO io
  => Table.Row Process
  -> GetProcessBuffer
  -> (Table.Row Buffer -> IO a)
  -> io (Either Strict.Text a)
processGetCaptureBuffer row which f = liftIO $
  let err msg = pure $ Left $ "process " <> Strict.pack (show $ Table.theRowLabel row) <> msg in
  let (Process mvar) = Table.theRowObject row in
  withMVar mvar $ \ cfg -> case evalGetProcessBuffer which $ theProcConfigState cfg of
    Nothing -> err " not running"
    Just pipe ->
      let ok = Right <$> f (pipeConnectBuffer pipe) in
      case pipe of
        PipeReceiverBuffer{} -> ok
        PipeReceiverNewBufferWithFile{} -> ok
        _ -> err " has no capture buffer"

-- | Obtain lines of text from the 'ProcessSTDOUT' or 'ProcessSTDERR' of a 'Process' that have been
-- cached in a buffer.
processGetLog
  :: MonadIO io
  => Table.Row Process
  -> GetProcessBuffer
  -> LineIndex
  -> Maybe LineIndex
  -> io (Either Strict.Text ProcessLog)
processGetLog row which fromLine toLine =
  processGetCaptureBuffer row which $ \ buf ->
  fmap
  (\ case
    Left err -> ProcessTextError err
    Right ok -> ok
  ) $
  withBuffer buf $
  TextRange <$>
  validateBounds fromLine <*>
  maybe maxLineIndex validateBounds toLine >>= \ range ->
  mapRangeFreeze range (const pure) >>= \ vec ->
  pure ProcessLog
  { theProcessLogHandle = row
  , theProcessLogRange = range
  , theProcessLog = vec
  }

----------------------------------------------------------------------------------------------------

-- | This is the monad that contains the global state, including a list of all buffers, and a list
-- of all processes and threads created by the APIs in this module. It is the global runtime
-- environment.
newtype Manager a = Manager (ReaderT ManagerEnv (StateT ManagerEnvState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader ManagerEnv)

-- | Provided a 'ManagerEnv' execution environment to keep all mutable state, evaluate a 'Manager'
-- function within the @IO@ context. Any changes made to the 'ManagerEnv' by evaluation of the 'Manager'
-- function will be remembered and visible to any later call to a 'Manager' function with that same
-- 'ManagerEnv'.
--
-- __DANGER:__ never run this function from within 'liftIO' in another 'Manager' monad, it will almost
-- certainly result in deadlocks, since this function itself obtains a lock and does not release it
-- until evaluation of the 'Manager' function completes. The only exception is if you evaluate this
-- function using @'liftIO' . 'forkIO'@, or if you are evaluating an entirely different 'ManagerEnv'
-- from the 'ManagerEnv' context of the current 'Manager' monad.
runManager :: Manager a -> ManagerEnv -> IO a
runManager (Manager (ReaderT f)) env@(ManagerEnv mvar) =
  modifyMVar mvar $ fmap (\ (a,b) -> (b,a) ) . runStateT (f env)

-- | Not for export
--
-- Lift a function of type @('StateT' 'ManagerEnvState' IO a)@ into a 'Manager' function type.
managerState :: StateT ManagerEnvState IO a -> Manager a
managerState = Manager . lift

-- | Values of this type maintain mutable data in an execution environment for evaluating functions
-- of type 'Manager', it contains tables of elements that should not be lost even if their handles go
-- out of scope. This data type locks all data shared between threads within an 'MVar' to ensure
-- thread safety.
newtype ManagerEnv = ManagerEnv (MVar ManagerEnvState)

-- | This is the mutable state data held within the 'ManagerEnv'. It is possible to perform read-only
-- operations on this data indirectly with functions like 'listManagerEnv', which can perform 'Table'
-- opereations on the data within. There are 3 tables stored within: the 'bufferTable' holds
-- in-memory data buffers, the 'workerTable' stores managed threads, and the 'processTable' stores
-- child processes.
data ManagerEnvState
  = ManagerEnvState
    { theBufferTable :: !(Table Buffer)
    , theWorkerTable :: !(Table Worker)
    , theProcessTable :: !(Table Process)
    }

-- | Create an execution environment shared by all calls to functions in the 'Manager' monad, and
-- across all threads. (It is thread-safe). Any changes made to this execution environment by
-- evaluation of a 'Manager' function will be remembered, and visible to later calls of other 'Manager'
-- functions.
newManagerEnv :: IO ManagerEnv
newManagerEnv = do
  buffers <- Table.new 128
  workers <- Table.new 32
  processes <- Table.new 32
  fmap ManagerEnv $ newMVar ManagerEnvState
    { theBufferTable = buffers
    , theWorkerTable = workers
    , theProcessTable = processes
    }

-- | The table of 'Worker's stored within the 'ManagerEnv'.
workerTable :: Lens' ManagerEnvState (Table Worker)
workerTable = lens theWorkerTable $ \ a b -> a{ theWorkerTable = b }

-- | The table of data 'Buffer's stored within the 'ManagerEnv'.
bufferTable :: Lens' ManagerEnvState (Table Buffer)
bufferTable = lens theBufferTable $ \ a b -> a{ theBufferTable = b }

-- | The table of child 'Process's stored within the 'ManagerEnv'.
processTable :: Lens' ManagerEnvState (Table Process)
processTable = lens theProcessTable $ \ a b -> a{ theProcessTable = b }

-- | not for export
--
-- Performs an arbitray update on any of the 'Table's in the 'ManagerEnv'.
editManagerEnvTable :: Lens' ManagerEnvState (Table thing) -> Table.Edit thing a -> Manager a
editManagerEnvTable table = Manager . lift . Table.withinGroup table

-- | List one of the 'Table's in the 'ManagerEnv': this could be 'bufferTable', 'threadTable', or
-- 'processTable'.
listManagerEnv
  :: Lens' ManagerEnvState (Table elem)
  -> (Table.Row elem -> IO Bool)
  -> Manager (Vector (Table.Row elem))
listManagerEnv inTable =
  editManagerEnvTable inTable .
  Table.list

-- | Print to stdout one of the 'Table's in the 'ManagerEnv': this could be 'bufferTable',
-- 'threadTable', or 'processTable'.
printManagerEnv :: DisplayInfo elem => Lens' ManagerEnvState (Table elem) -> Manager ()
printManagerEnv inTable =
  editManagerEnvTable inTable $
  Table.printRows Strict.putStr

-- | Print a list of all data 'Buffer's in the 'ManagerEnv' that were created by 'newBuffer'.
bufferPrintAll :: Manager ()
bufferPrintAll = printManagerEnv bufferTable

-- | Print a list of all data 'Buffer's in the 'ManagerEnv' that were created by 'newBuffer'.
bufferList :: (Table.Row Buffer -> IO Bool) -> Manager (Vector (Table.Row Buffer))
bufferList = listManagerEnv bufferTable

-- | Print a list of all managed 'Worker' threads that are known to the 'ManagerEnv', meaning they were
-- created by the 'startWork' function. The 'ManagerEnv' execution context cannot track threads created
-- by 'forkIO' or 'forkOS'.
workerPrintAll :: Manager ()
workerPrintAll = printManagerEnv workerTable

workerList :: (Table.Row Worker -> IO Bool) -> Manager (Vector (Table.Row Worker))
workerList = listManagerEnv workerTable

-- | Prints a list of all child 'Process's that are known to the 'ManagerEnv' created by
-- 'runProcessWithBuffer'. The 'ManagerEnv' execution context cannot track processes created by
-- 'createProcess' or any of its convenient functional wrappers.
processPrintAll :: Manager ()
processPrintAll = printManagerEnv processTable

-- | Construct a list (actually 'Vector') of 'Process's from the process table that match the
-- given predicate.
processList :: (Table.Row Process -> IO Bool) -> Manager (Vector (Table.Row Process))
processList = editManagerEnvTable processTable . Table.list 

hackEnvTableSelect1
  :: Lens' ManagerEnvState (Table elem)
  -> (Table.Row elem -> Bool)
  -> Manager (Maybe (Table.Row elem))
hackEnvTableSelect1 inTable testElem =
  editManagerEnvTable inTable $
  Table.select1 testElem

-- | Create a new 'Worker', passing a 'Label' to identify the worker in the 'workerList' table, and
-- a task of type @IO ()@ to perform.
--
-- This function returns a 'Table.Row' because, like with 'Buffer's, any new 'Worker' thread created
-- has a handle for it stored in a table in the 'Manager' monad execution environment so it can be
-- retrieved at any time.
startWork
  :: Table.Label
  -> (Table.Row Worker -> IO (Either EditTextError ()))
  -> Manager (Table.Row Worker)
startWork label task =
  ask >>= \ env ->
  editManagerEnvTable workerTable $ do
    selfMVar <- liftIO $ newEmptyMVar
    statMVar <- liftIO $ newMVar Ready
    thid <-
      liftIO $
      forkIO $
      (do modifyMVar_ statMVar (pure . const Working)
          self   <- takeMVar selfMVar
          result <- task self
          runManager (clearWorker self) env
          modifyMVar_ statMVar $
            const $ pure $
            case result of
              Left err -> WorkerError err
              Right () -> Success
      )
      `catch` \ err ->
        modifyMVar_ statMVar (pure . const (WorkerFailed err))
    let worker = Worker
          { theWorkerId = thid
          , theWorkerStatus = statMVar
          }
    row <- Table.insert label worker
    liftIO $ putMVar selfMVar row
    pure row

-- | not for export
--
-- Remove a worker from the 'globalBufferTable'
clearWorker :: Table.Row Worker -> Manager Bool
clearWorker worker =
  editManagerEnvTable workerTable $
  Table.remove1 (Table.byRowValue (== (worker ^. Table.rowValue)))

-- | Create a new data 'Buffer'. Thhis function also registers the buffer into the 'bufferTable' of
-- the 'ManagerEnv' execution environment, which is why it returns a 'Table.Row'. This is so if you
-- lose the handle to it while working in GHCI, you can get it back using 'getBuffer'.
newBuffer :: Table.Label -> VectorSize -> Manager (Table.Row Buffer)
newBuffer = (managerState .) . internalNewBuffer

internalNewBuffer :: Table.Label -> VectorSize -> StateT ManagerEnvState IO (Table.Row Buffer)
internalNewBuffer label initSize =
  Table.withinGroup bufferTable $
  ( liftIO $
    newEditTextState initSize >>= \ ed ->
    fmap Buffer $
    newMVar $
    BufferState
    { theBufStateFilePath = ""
    , theBufStateLockedBy = Nothing
    , theBufStateBuffer   = ed
    }
  ) >>=
  Table.insert label

withBufferState :: MonadIO io => StateT BufferState IO a -> Buffer -> io a
withBufferState f (Buffer mvar) =
  liftIO $
  modifyMVar mvar $
  fmap (\ (a, b) -> (b, a)) . runStateT f

withBuffer0 :: MonadIO io => Buffer -> EditText TextTags a -> io (Either EditTextError a)
withBuffer0 buffer f =
  flip withBufferState buffer $ do
    ed <- use bufStateBuffer
    (a, ed) <- liftIO $ runEditText f ed
    bufStateBuffer .= ed
    pure a

-- | Manipulate a buffer if you have it's handle already.
withBuffer :: MonadIO io => Table.Row Buffer -> EditText TextTags a -> io (Either EditTextError a)
withBuffer = withBuffer0 . Table.theRowObject

-- | Pretty-print each line of data in a 'Buffer'.
bufferShow :: MonadIO io => Table.Row Buffer -> TextRange LineIndex -> io (Either EditTextError ())
bufferShow row range =
  withBuffer row $
  foldLinesInRange range
  (\ _halt lineNum line -> liftIO $ do
     putStr (ralign6 lineNum)
     Strict.putStr ": "
     putStrLn (convertString (theTextLineData line))
     pure Nothing
  )
  ()

-- | Change the file path to which a buffer will be written when saved.
withBufferPath :: MonadIO io => Table.Row Buffer -> (Strict.Text -> Strict.Text) -> io ()
withBufferPath buf f = flip withBufferState (Table.theRowObject buf) $ bufStateFilePath %= f

-- | Read entire content of a 'Handle' into a 'Buffer'. The 'Label' here is for what the 'Worker'
-- should be called.
bufferHandle :: Table.Label -> Table.Row Buffer -> Handle -> Manager (Table.Row Worker)
bufferHandle label row handle =
  startWork label $ \ _worker ->
  withBuffer row $
  loadHandleEditText Nothing handle

-- | Load a file from the given file path into the given 'Buffer'. Returns the 'Worker' that was
-- used to load the buffer.
bufferFileInto :: Strict.Text -> Table.Row Buffer -> Manager (Table.Row Worker)
bufferFileInto path buffer = do
  handle <- liftIO $ openFile (Strict.unpack path) ReadMode
  worker <- bufferHandle ("(bufferFile " <> Strict.pack (show path) <> ")") buffer handle
  return worker

-- | Like 'bufferFileInto' but creates a new buffer and returns it paired with the 'Worker' that
-- buffers the file.
bufferFile :: Strict.Text -> Manager (Table.Row Worker, Table.Row Buffer)
bufferFile path = do
  buffer <- newBuffer path 128
  worker <- bufferFileInto path buffer
  return (worker, buffer)

----------------------------------------------------------------------------------------------------

-- | When searching throught the global table, you can select items by one of 2 types of index
-- values: 1. their unique id, or 2. by their nickname. The 'getBuffer' and 'getWorker' functions
-- are polymorphic over the search index so you can use either with the same function.
class GlobalTableSearchIndex i where
  -- | Search for a 'BufferState' created by 'newBuffer'.
  getBuffer :: i -> Manager (Table.Row Buffer)
  -- | Search for a 'ThreadTableElem' created by 'newThread'.
  getWorker :: i -> Manager (Table.Row Worker)

instance GlobalTableSearchIndex Int where
  getBuffer index =
    hackEnvTableSelect1 bufferTable (Table.byRowId index) >>=
    maybe (error $ "no buffer identified by index " <> show index) pure
  getWorker index =
    hackEnvTableSelect1 workerTable (Table.byRowId index) >>=
    maybe (error $ "no worker identified by index " <> show index) pure

instance GlobalTableSearchIndex (Strict.Text -> Bool) where
  getBuffer label =
    hackEnvTableSelect1 bufferTable (Table.byRowLabel label) >>=
    maybe (error $ "no buffer matching label predicate") pure
  getWorker label =
    hackEnvTableSelect1 workerTable (Table.byRowLabel label) >>=
    maybe (error $ "no worker matching label predicate") pure

instance GlobalTableSearchIndex Strict.Text where
  getBuffer label =
    hackEnvTableSelect1 bufferTable (Table.byRowLabel (== label)) >>=
    maybe (error $ "no buffer matching label " <> show label) pure
  getWorker label =
    hackEnvTableSelect1 workerTable (Table.byRowLabel (== label)) >>=
    maybe (error $ "no worker matching label " <> show label) pure
