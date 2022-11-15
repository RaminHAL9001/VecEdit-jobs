-- | A job manager. A table of child processes is maintained, the
-- @STDIN@, @STDOUT@, and @STDERR@ of each process is captured in a
-- text buffer.
module VecEdit.Jobs
  ( -- ** The 'Manager' monad
    Manager, ManagerEnv, newManagerEnv, runManager,
    ManagerEnvState, bufferTable, workerTable, processTable,

    -- ** Text 'Buffer's
    Buffer, TextTags, newBuffer, newBufferSized,
    bufferPrintAll, bufferList, bufferHandle, bufferFile, bufferFileInto, bufferShow,
    withBuffer, withBufferPath,

    -- ** 'Worker's (threads)
    Worker, WorkerStatus, startWork, haltWorker,
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
    ProcessConfig(..),
    ProcessProgramFile, ProcessState,
    procConfigCommand, procConfigArgs,
    PipeControlInitializer, ControlThreadErrorConfig(..), controlThreadErrorConfig,

    -- *** Configuring how data is sent to and received from a child process.
    PipeSenderConfig(..), PipeReceiverConfig(..),
    synchronous, asynchronous, asyncCapture, asyncCaptureNewBuffer, asyncLineFilter,

    -- *** Custom 'PipeReceiverConfigFunction' configurations
    pipeBufferLines, pipeMapLines, pipeFoldLines, pipeMapBufferLines, pipeFoldBufferLines,
    
    -- *** Running a 'Process' as a job
    Process, newProcess, runProcess,
    processGetConfig, ProcessStateInfo(..), processStateInfo,
    processPrintAll, processList, processFindById,
    processSend, processWait, processCancel, processRemove, processRemoveAll,
    processGetCaptureBuffer,
    GetProcessBuffer(..), ProcessLog(..), processGetLog,

    -- ** Inspecting global state
    GlobalTableSearchIndex(..),
  ) where

import VecEdit.Types
  ( RelativeDirection(..), TextRange(..), LineIndex(..),
    LineBufferSize, CharBufferSize, EditTextError(..),
  )

import VecEdit.Print.DisplayInfo (DisplayInfo(..), displayInfoShow, showAsText, ralign6)
import VecEdit.Text.Line.Break (LineBreakSymbol)
import VecEdit.Text.Line
  ( TextLine, StringData,
    textLineString, textLineBreakSymbol, fromStringData,
  )
import VecEdit.Text.Line.Editor
  ( LineEditor(..), EditLine,
    newEditLineState, lineBreak, streamEditorStack, copyBufferClear,
  )
import VecEdit.Text.Stream
  ( EditStream, HaltEditStream,
    newEditStreamState, editStreamState, hFoldLines,
  )
import VecEdit.Text.Editor
  ( EditText, EditTextState,
    newEditTextState, runEditText, foldLinesInRange,
    maxLineIndex, validateBounds, mapRangeFreeze,
  )
import qualified VecEdit.Table as Table
import VecEdit.Table (Table)

import Control.Arrow ((|||), (>>>))
import Control.Concurrent (ThreadId, forkIO, yield)
import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar, readMVar, modifyMVar, modifyMVar_,
    takeMVar, putMVar, withMVar,
  )

import Control.Exception
  ( SomeException, AsyncException(UserInterrupt), catch, throwTo
  )
import Control.Lens (Lens', lens, use, (^.), (.~), (%=), (.=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (MonadReader, ask, ReaderT(..))
import Control.Monad.State (StateT(..), MonadState(..), runStateT, execStateT)
import Control.Monad.Trans (lift)

import Data.IORef (IORef, readIORef, writeIORef)
import Data.Function (on)
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Vector as Vec
import Data.Vector (Vector)

import System.IO
  ( Handle, IOMode(..), openFile, hIsClosed, hClose,
    hGetContents, hFlush, hPrint, hPutStr, hPutStrLn, stderr
  )
import qualified System.Process as Exec
import System.Process
  ( ProcessHandle,
    waitForProcess, getProcessExitCode, interruptProcessGroupOf
  )
import System.Exit (ExitCode(..))
import qualified System.Posix.Signals as POSIX
import System.Posix.Signals (sigABRT, sigKILL)

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

instance Show Worker where
  show (Worker{theWorkerId=wid}) =
    "(Worker" <>
    dropWhile (/= ' ') (show wid) <>
    ")"

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

-- | Configure how a child process is to be launched and how IO to and from this child process
-- should be controlled. Use functions such as 'synchronous', 'asynchronous', 'asyncCapture',
-- 'asyncCaptureNewBuffer', or 'asyncLineFilter' to setup the IO for the process, and then set
-- 'theProcConfigCommand' and 'theProcConfigArgs' on the value returned by one of the above
-- mentioned functions.
data ProcessConfig
  = ProcessConfig
    { theProcConfigCommand :: !ProcessProgramFile
    , theProcConfigArgs    :: !(Vector Strict.Text)
    , theProcConfigInPipe  :: !PipeSenderConfig
    , theProcConfigOutPipe :: !PipeReceiverConfig
    , theProcConfigErrPipe :: !PipeReceiverConfig
    , theProcConfigState   :: !ProcessState
    }

procConfigCommand :: Lens' ProcessConfig ProcessProgramFile
procConfigCommand = lens theProcConfigCommand $ \ a b -> a{ theProcConfigCommand = b }

procConfigArgs :: Lens' ProcessConfig (Vector Strict.Text)
procConfigArgs = lens theProcConfigArgs $ \ a b -> a{ theProcConfigArgs = b }

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
-- process will be used. These are primitive functions that can setup any kind of IO between this
-- process and the child process, it is better to use less primitive functions such as
-- 'asynchronous', 'asyncCapture', 'asyncCaptureNewBuffer', or 'asyncLineFilter' rather than try to
-- set 'theProcConfigOutPipe' and 'theProcConfigErrPipe' directly using one of these primitives.
data PipeReceiverConfig
  = PipeReceiverConfigClose -- ^ Close the stream after forking the child process.
  | PipeReceiverConfigInherit -- ^ Inherit the pipe from the current process
  | PipeReceiverConfigNewBuffer
    { pipeControlOnError :: !ControlThreadErrorConfig
    } -- ^ Launch a thread to capture the 'Handle' into a newly created 'Buffer'.
  | PipeReceiverConfigBuffer
    { pipeControlBuffer :: !(Table.Row Buffer)
    , pipeControlOnError :: !ControlThreadErrorConfig
    }  -- ^ Launch a thread to capture the 'Handle' into the given 'Buffer'.
  | PipeReceiverConfigHandle
    { pipeControlHandle :: !Handle
    } -- ^ Redirect to an already-open 'Handle'. Redirection of bytes in the stream is controlled by
      -- the OS, no thread is created to do this.
  | PipeReceiverConfigFile
    { pipeControlFile :: !Strict.Text
    , pipeControlAppend :: !Bool
      -- ^ 'True' -> 'AppendMode', 'False' -> 'WriteMode' (truncate)
    } -- ^ Redirect to a 'Handle' by opening the given file path. Redirection of bytes in the stream
      -- is controlled by the OS, no thread is created to do this.
  | PipeReceiverConfigNewBufferWithFile
    { pipeControlFile :: !Strict.Text
    , pipeControlOnError :: !ControlThreadErrorConfig
    , pipeControlAppend :: !Bool
      -- ^ 'True' -> 'AppendMode', 'False' -> 'WriteMode' (truncate)
    } -- ^ Launch a thread to capture the 'Handle' into a 'Buffer', but also write captured output
      -- to a file.
  | PipeReceiverConfigFunction
    { pipeControlDescription :: !Strict.Text
    , pipeControlOnError :: !ControlThreadErrorConfig
    , pipeControlFunction :: PipeControlInitializer
    } -- ^ Launch a thread to read or write the 'Handle'. Provide a descriptive string, otherwise
     -- what this function actually does with the 'Handle' is completely opaque. For all other
     -- stream receiver/sender mechanisms that are not handled by the other constructors of this
     -- data type, you can define your own with this constructor. You can of course also create a
     -- text buffer a evaluate a 'EditStream' loop on the 'Handle', if you do not like the way
     -- 'PipeReceiverConfigBuffer' does it.

-- | Functions of this type are initialzers for a function that reads bytes from a file
-- 'Handle'. When creating a control thread for reading bytes that are from the pipe of a child
-- process, this function is evaluated first thing when the control thread is forked. This function
-- can setup whatever state is necessary (e.g. creating new 'IORef's). As long as this state is
-- private to the closure it is guaranteed to be accessible only to a single thread. The returned
-- closure is used as a continuation, and is passed the pipe 'Handle', where the closure can begin
-- looping over reading bytes from the 'Handle'.
-- 
-- Functions of this type are usually used to define a 'PipeReceiverConfigFunction' for the
-- 'theProcConfigOutPipe' or 'theProcConfigErrPipe' of a 'ProcessConfig'. The
-- 'PipeControlInitializer' will usually setup a loop over the pipe 'Handle' to receive the byte
-- stream from a child process, and break the byte stream up into 'TextLine's. The 'pipeFoldLines',
-- 'pipeMapLines', 'pipeBufferLines', and 'pipeFoldBufferLines' are all initializers of this
-- function type.
type PipeControlInitializer = IO (Handle -> IO (Either EditTextError ()))

data ControlThreadErrorSignal = CtrlErrNoSignal | CtrlErrSIGABRT | CtrlErrSIGKILL
  deriving (Eq, Ord, Enum)

instance Show ControlThreadErrorSignal where
  show = \ case
    CtrlErrNoSignal -> "#f"
    CtrlErrSIGABRT  -> "'ABRT"
    CtrlErrSIGKILL  -> "'KILL"

-- | When a child process is launched, local threads are also launched to recieve information that
-- the child process emits on it's 'stdout' and 'stderr' channels. These threads run computations
-- that typically inspect the byte stream and buffer the bytes. These computations may fail,
-- however, and if a thread that was controlling the child process output fails, a decision must be
-- made as to what should be done. The default constructor is 'controlThreadErrorConfig'.
--
-- The order in which error event handling actions occur is this:
--
--   1. If the number of errors occured has not exceeded 'onErrorRetryTimes' then evaluate
--      the 'PipeControlInitializer' again and try to resume reading input
--
--   2. 
data ControlThreadErrorConfig
  = ControlThreadErrorConfig
    { onErrorVentHandle :: !Bool
      -- ^ Enter into a loop that takes all data from the process 'Handle' and deletes it. This is
      -- 'False' by default.
    , onErrorPrintStderr :: !Bool
      -- ^ Try to print the error message to 'stderr'. This is 'True' by default.
    , onErrorCloseHandle :: !Bool
      -- ^ Try to close the handle from the child process. This will likely cause the operating
      -- system to send the @SIGPIPE@ signal, which terminates the process same as @SIGKILL@. This
      -- might be a good way to indicate the nature of the failure. This is 'True' by default.
    , onErrorSignalChild :: !ControlThreadErrorSignal
      -- ^ Send a signal to the child process. This is 'CtrlErrSIGABRT' by default.
    , onErrorRetryTimes  :: !Int
      -- ^ Try running the 'PipeControlInitializer' again and resume handling input. Repeat up to
      -- @n@ times before performing actions to terminate the child process. This is 0 (zero) by
      -- default.
    }

instance Show ControlThreadErrorConfig where
  show cfg = let bool test sym = if test cfg then ' ' : sym else "" in
    "(on-error" <> bool onErrorVentHandle "#:vent" <>
    bool onErrorPrintStderr "#:print-stderr" <>
    bool onErrorCloseHandle "#:close-handle" <>
    ( case onErrorSignalChild cfg of
        CtrlErrNoSignal -> ""
        sig -> ' ' : show sig
    ) <>
    " #:retry " <> show (onErrorRetryTimes cfg) <>
    ")"

instance DisplayInfo ControlThreadErrorConfig where { displayInfo = displayInfoShow; }

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
    , pipeConnectBuffer :: !(Table.Row Buffer)
    , pipeConnectThread :: !ThreadId
    , pipeConnectWaitClose :: !(MVar (Either EditTextError ()))
    }
  | PipeReceiverFunction
    { pipeConnectDescription :: !Strict.Text
    , pipeConnectHandle :: !Handle
    , pipeConnectThread :: !ThreadId
    , pipeConnectWaitClose :: !(MVar (Either EditTextError ()))
    } -- ^ For all other stream receiver mechanisms that are not handled by the other constructors
      -- of this data type, you can define your own with this constructor.

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

-- | An image of a 'ProcessState' that contains no private information and can be safely
-- deconstructed. You can obtain this information from a 'Process' using 'processStateInfo',
data ProcessStateInfo
  = ProcInfoPending
    -- ^ The 'ProcessConfig' has been configured but has never been run.
  | ProcInfoRunning
    { theProcInfoPid :: !Exec.Pid
    , theProcInfoStartTime :: !UTCTime
    } -- ^ The process is running but has not halted yet.
  | ProcInfoHalted
    { theProcInfoExitCode :: !ExitCode
    , theProcInfoStartTime :: !UTCTime
    , theProcInfoEndTime :: !UTCTime
    } -- ^ The process has halted.

-- | Get the 'ProcessStateInfo' for a particular 'ProcessConfig', usually obtained from the
processStateInfo :: ProcessConfig -> ProcessStateInfo
processStateInfo = theProcConfigState >>> \ case
  ProcPending -> ProcInfoPending
  ProcRunning
   {theProcStatePid=pid
   ,theProcStartTime=start
   } ->
    ProcInfoRunning
    { theProcInfoPid = pid
    , theProcInfoStartTime = start
    }
  ProcHalted
   {theProcStateExitCode=exitCode
   ,theProcStartTime=start
   ,theProcEndTime=end
   } ->
    ProcInfoHalted
    { theProcInfoExitCode = exitCode
    , theProcInfoStartTime = start
    , theProcInfoEndTime = end
    }

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
    PipeReceiverConfigNewBuffer onErr ->
      "(capture-new-buffer :on-error " <> show onErr <> ")"
    PipeReceiverConfigBuffer buf onErr ->
      "(capture-buffer " <> show (Table.theRowLabel buf) <>
      " :on-error " <> show onErr <> ")"
    PipeReceiverConfigHandle _ -> "(file-handle)"
    PipeReceiverConfigFile file _ -> "(file " <> show file <> ")"
    PipeReceiverConfigNewBufferWithFile file onErr append ->
      "(capture-buffer " <> show file <>
      (if append then ":append-mode" else "") <>
      " :on-error " <> show onErr <> ")"
    PipeReceiverConfigFunction desc onErr _ ->
      "(capturing " <> show desc <> " :on-error " <> show onErr <> ")"

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

-- | This function performs some clean-up action if a Haskell exception is thrown in the thread that
-- is handling one of the child process streams (the child's 'stdout' or 'stderr' stream).
evalControlThreadErrorConfig
  :: ControlThreadErrorConfig
  -> ProcessState
  -> Handle
  -> (Int -> IO (Either EditTextError ()))
  -> Int
  -> SomeException
  -> IO (Either EditTextError ())
evalControlThreadErrorConfig cfg procSt procHandle loop i err =
  if i >= onErrorRetryTimes cfg then abort else
  hIsClosed procHandle >>= \ isClosed ->
  if isClosed then return errMessage else
  ( ifEnabled onErrorPrintStderr $ do
      hPrint stderr err
      hPutStrLn stderr $ "retry " <> show i <> ('/' : show (onErrorRetryTimes cfg))
  ) >>
  (loop $! i + 1)
  where 
  errMessage = Left $ EditTextFailed $ Strict.pack $ show procSt <> (' ' : show err)
  ifEnabled test action = if test cfg then action else pure ()
  abort = do
    ifEnabled onErrorPrintStderr $
      hPrint stderr $ show procSt <> (' ' : show err)
    ifEnabled onErrorCloseHandle $
      hIsClosed procHandle >>= \ isClosed ->
      if isClosed then pure () else hClose procHandle
    case procSt of
      ProcRunning{ theProcStatePid=pid } ->
        case onErrorSignalChild cfg of
          CtrlErrNoSignal -> pure ()
          CtrlErrSIGABRT  -> POSIX.signalProcess sigABRT pid
          CtrlErrSIGKILL  -> POSIX.signalProcess sigKILL pid
      _ -> pure ()
    ifEnabled onErrorVentHandle $
      const () <$> hGetContents procHandle
    pure errMessage

-- | This is the default 'ControlThreadErrorConfig'.
controlThreadErrorConfig :: ControlThreadErrorConfig
controlThreadErrorConfig =
  ControlThreadErrorConfig
  { onErrorVentHandle = False
  , onErrorPrintStderr = True
  , onErrorCloseHandle = True
  , onErrorSignalChild = CtrlErrSIGABRT
  , onErrorRetryTimes = 0
  }

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

-- | This function sets up a loop over the pipe 'Handle' to receive the byte stream from a child
-- process, and break the byte stream up into 'TextLine's. Each 'TextLine' received inserted into
-- the given 'Buffer' at that buffer's current cursor position.
--
-- This function is used when you setup a 'PipeReceiverConfig' using either of the
-- 'PipeReceiverConfigNewBuffer' or 'PipeReceiverConfigBuffer' constructors.
pipeBufferLines :: Table.Row Buffer -> PipeControlInitializer
pipeBufferLines row = pipeMapBufferLines row (\ _ _ -> pure ())

-- | Used to define 'pipeFoldLines' and 'pipeMapLines' __without__ a backing 'TextBuffer'. This
-- function evaluates 'hFoldLines' within 'foldEditLines'
pipeFoldMapLines
  :: (ref -> EditLine tags fold)
  -> (ref -> fold -> EditLine tags ())
  -> CharBufferSize
  -> ref
  -> (HaltEditStream fold tags -> LineBreakSymbol -> EditStream fold tags ())
  -> PipeControlInitializer
pipeFoldMapLines read write bufsiz foldref useLine =
  pure $ \ pipeHandle ->
  fmap fst $
  newEditLineState bufsiz >>=
  streamEditorStack
  (const $ pure ()) -- discard all lines emitted
  ( read foldref >>=
    newEditStreamState >>=
    fmap fst . hFoldLines pipeHandle
    (\ halt lbrk ->
      useLine halt lbrk >>
      get >>= liftEditLine . write foldref
    ) >>=
    (throwError ||| pure)
  )

-- | This function sets up a loop over the pipe 'Handle' to receive the byte stream from a child
-- process, and break the byte stream up into 'TextLine's. Each 'TextLine' received is passed to the
-- 'EditStream' continuation given here.
--
-- The 'EditStream' function has no backing 'EditTextState', so you do not have random access to
-- lines in a buffer (to do this, use 'pipeMapBufferLines'). However, evaluating 'newline' or
-- 'pushLine' will push the content of the current line editor onto a stack and can be pulled with
-- 'joinLine'.
pipeMapLines
  :: CharBufferSize
  -> (HaltEditStream () tags -> LineBreakSymbol -> EditStream () tags ())
  -> PipeControlInitializer
pipeMapLines bufsiz = pipeFoldMapLines (\ () -> pure ()) (\ () () -> pure ()) bufsiz ()

-- | similar to 'pipeMapLines', this function sets up a loop over the pipe 'Handle' to receive the
-- byte stream from a child process, and break the byte stream up into 'TextLine's. Each 'TextLine'
-- received is passed to the 'EditStream' continuation given here. Unlike 'pipeMapLines', this
-- function may also update an arbitrary @fold@ value in an 'IORef' that you must create for use
-- with this function.
--
-- The 'EditStream' function has no backing 'EditTextState', so although you may edit each
-- individual 'TextLine', evaluating functions such as 'newline', 'pushLine', or 'joinLine' will
-- cause the loop to end evaluation on that 'TextLine' and iterate on the next incoming 'TextLine'.
pipeFoldLines
  :: CharBufferSize
  -> IORef fold
  -> (HaltEditStream fold tags -> LineBreakSymbol -> EditStream fold tags ())
  -> PipeControlInitializer
pipeFoldLines = pipeFoldMapLines (liftIO . readIORef) (\ ref -> liftIO . writeIORef ref)

-- | Used to define 'pipeFoldBufferLines' and 'pipeMapBufferLines'.
pipeFoldMapBufferLines
  :: (ref -> EditLine TextTags fold)
  -> (ref -> fold -> EditLine TextTags ())
  -> ref
  -> Table.Row Buffer
  -> (HaltEditStream fold TextTags -> LineBreakSymbol -> EditStream fold TextTags ())
  -> PipeControlInitializer
pipeFoldMapBufferLines read write foldref row useLine =
  pure $ \ pipeHandle ->
  fmap join $
  withBuffer row $
  liftEditLine $
  read foldref >>=
  newEditStreamState >>=
  hFoldLines pipeHandle useLine >>= \ (result, readst) ->
  write foldref (readst ^. editStreamState) >>
  pure result

-- | This function sets up a loop over the pipe 'Handle' to receive the byte stream from a child
-- process, and break the byte stream up into 'TextLine's. Each 'EditStream' function is evaluated
-- on a 'EditLineState' buffer containing the content of the pipe between line breaking symbols. The
-- cursor is at the end of the buffer, and you can retrieve a 'TextLine' using @('cutLine'
-- 'Before')@.
--
-- This function is used when you setup a 'PipeReceiverConfig' using the
-- 'PipeReceiverConfigNewBufferWithFile' constructors.
--
-- __WARNING__: if evaluating this function on a 'Buffer', this will lock the buffer until the
-- entire buffer is full, so it will not be accessible until after the process completes
-- execution. This is slightly faster, but it can hang your program for a while if you attempt to
-- access the given 'Buffer' while the chile process is running.
pipeMapBufferLines
  :: Table.Row Buffer
  -> (HaltEditStream () TextTags -> LineBreakSymbol -> EditStream () TextTags ())
  -> PipeControlInitializer
pipeMapBufferLines =
  pipeFoldMapBufferLines (\ () -> pure ()) (\ () () -> pure ()) ()

-- | This function sets up a loop over the pipe 'Handle' to receive the byte stream from a child
-- process, and break the byte stream up into 'TextLine's. Each 'TextLine' received is passed to the
-- 'EditStream' continuation given here. It combines the functionality of 'pipeFoldLines' and
-- 'pipeBufferLines', not only evaluating the given continuation function, but also buffering each
-- 'TextLine' received into the given 'Buffer'. Buffering happens after evaluating the continuation,
-- so any edits made to the 'TextLine' can be written to the buffer.
--
-- __WARNING__: if evaluating this function on a 'Buffer', this will lock the buffer until the
-- entire buffer is full, so it will not be accessible until after the process completes
-- execution. This is slightly faster, but it can hang your program for a while if you attempt to
-- access the given 'Buffer' while the child process is running.
pipeFoldBufferLines
  :: IORef fold
  -> Table.Row Buffer
  -> (HaltEditStream fold TextTags -> LineBreakSymbol -> EditStream fold TextTags ())
  -> PipeControlInitializer
pipeFoldBufferLines =
  pipeFoldMapBufferLines (liftIO . readIORef) (\ ref -> liftIO . writeIORef ref)

-- | Inspect a 'PipeReceiverConfig' and return an 'Exec.StdStream' value that can be used to
-- construct a 'PipeReceiver', either with 'connectPipeReceiver' or 'connectPipeSender'. This
-- function attempts to open a file if a 'PipeReceiverConfigFile' or
-- 'PipeReceiverConfigNewBufferWithFile' is given. This sets up redirection to a file by opening a
-- file 'Handle', or redirectinng to an existing 'Handle'.
pipeReceiverConfigStdStream :: PipeReceiverConfig -> IO Exec.StdStream
pipeReceiverConfigStdStream = \ case
  PipeReceiverConfigClose -> pure Exec.NoStream
  PipeReceiverConfigInherit -> pure Exec.Inherit
  PipeReceiverConfigNewBuffer{} -> pure Exec.CreatePipe
  PipeReceiverConfigBuffer{} -> pure Exec.CreatePipe
  PipeReceiverConfigFile path append ->
    Exec.UseHandle <$>
    openFile (Strict.unpack path) (if append then AppendMode else WriteMode)
  PipeReceiverConfigNewBufferWithFile{} -> pure Exec.CreatePipe
  PipeReceiverConfigFunction{} -> pure Exec.CreatePipe
  PipeReceiverConfigHandle{ pipeControlHandle=h } -> pure $ Exec.UseHandle h

-- | Evaluate a 'PipeReceiverConfig' to a readable 'Handle', from either a STDOUT or STDERR stream
-- of a child 'Process'. If information is to be received from a process via a new file 'Handle', a
-- controlling thread is launched which can wait on SIGCONT signals.
pipeReceiver
  :: Maybe Handle
  -> ProcessConfig
  -> PipeReceiverConfig
  -> StateT ManagerEnvState IO PipeReceiver
pipeReceiver procHandle cfg pipeControl =
  let requirePipe f =
        maybe
        ( error $ "internal error: 'pipeReceiver' evaluating 'ProcessConfig':\n  " <>
          show cfg <> "\n  should have received a pipe to the child process.\n" <>
          "  this may be a bug in 'pipeReceiverConfigStdStream'."
        ) f procHandle
      makeHandlerThread onErr procHandle initHandler = liftIO $ do
        -- Launch a thread for waiting on SIGCONT signals and reading the process STDOUT or
        -- STDERR. An mvar is created to use as a lock, which is unlocked when the process finishes.
        f <- initHandler
        waitMVar <- newEmptyMVar
        thid <- forkIO $
          ( let loop =
                  catch (f procHandle) .
                  evalControlThreadErrorConfig
                  onErr
                  (theProcConfigState cfg)
                  procHandle
                  loop
            in  loop 0
          ) >>=
          putMVar waitMVar
        pure (thid, waitMVar)
      makeConnectBuffer onErr readLoop row procHandle =
        -- Launch a thread and capture the process STDOUT or STDERR in a buffer
        makeHandlerThread onErr procHandle $ readLoop row
  in case pipeControl of
    PipeReceiverConfigClose -> pure PipeReceiverNothing
    PipeReceiverConfigInherit -> pure PipeReceiverInherit
    PipeReceiverConfigHandle h ->
      -- Here the 'Handle' h should be the same as 'procHandle'.
      pure $ PipeReceiverHandle{ pipeConnectHandle = h }
    PipeReceiverConfigFile path append ->
      requirePipe $ pure . PipeReceiverFile path append
    PipeReceiverConfigBuffer row onErr ->
      requirePipe $ \ procHandle -> do
        (thid, wait) <- makeConnectBuffer onErr pipeBufferLines row procHandle
        pure PipeReceiverBuffer
          { pipeConnectHandle = procHandle
          , pipeConnectBuffer = row
          , pipeConnectThread = thid
          , pipeConnectWaitClose = wait
          }
    PipeReceiverConfigNewBuffer onErr ->
      requirePipe $ \ procHandle -> do
        buf <- internalNewBuffer (showAsText cfg) Nothing Nothing
        (thid, wait) <- makeConnectBuffer onErr pipeBufferLines buf procHandle
        pure PipeReceiverBuffer
          { pipeConnectHandle = procHandle
          , pipeConnectBuffer = buf
          , pipeConnectThread = thid
          , pipeConnectWaitClose = wait
          }
    PipeReceiverConfigNewBufferWithFile path onErr append ->
      requirePipe $ \ procHandle -> do
        buf <- internalNewBuffer (showAsText cfg) Nothing Nothing
        logHandle <- liftIO $
          openFile (Strict.unpack path) (if append then AppendMode else WriteMode)
        (this, wait) <-
          makeConnectBuffer onErr
          ( flip pipeMapBufferLines $ \ _halt lbrk ->
            (textLineBreakSymbol .~ lbrk) <$> cutLine Before >>=
            liftIO . hPutStr logHandle . textLineString
          ) buf procHandle
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
    PipeReceiverConfigFunction desc onErr init ->
      requirePipe $ \ procHandle -> do
        (this, wait) <- makeHandlerThread onErr procHandle init
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
  -- Setup 'Handle' redirection, or else inherit from the current process.
  outHandle <- pipeReceiverConfigStdStream $ theProcConfigOutPipe cfg
  errHandle <- pipeReceiverConfigStdStream $ theProcConfigErrPipe cfg
  -- Construct and return a process configuration needed by "System.Process".
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
      -- If any redirection file handles need to be created, 'pipeReceiverConfigStdStream' does that
      -- here via 'processConfigCreate'.
      (pin, pout, perr, phandle) <- processConfigCreate cfg >>= Exec.createProcess
      -- Now the process *should* be running, and the file handles for pipes to the handles *should*
      -- exist.
      pid <- Exec.getPid phandle
      let inPipe = pipeSender pin $ theProcConfigInPipe cfg
      -- Setup the pipe receiver threads.
      ((outPipe, errPipe), mgrst) <- flip runStateT mgrst $
        (,) <$>
        pipeReceiver pout cfg (theProcConfigOutPipe cfg) <*>
        pipeReceiver perr cfg (theProcConfigErrPipe cfg)
      let waitForEnd = do
            exitCode <- waitForProcess phandle
            closePipeSender inPipe
            closePipeReceiver outPipe
            closePipeReceiver errPipe
            pure exitCode
      case pid of
        Nothing -> do
          -- Process forking failed.
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
          -- Process forking succeeded, create one more thread to wait on SIGCHLD signals, which
          -- updates the 'ProcessState' when the child process finishes.
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
runProcess = runProcess0 . Table.theRowValue

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
asynchronous :: ControlThreadErrorConfig -> ProcessProgramFile -> [Strict.Text] -> ProcessConfig
asynchronous onErr cmd args =
  (synchronous cmd args)
  { theProcConfigInPipe = PipeSenderConfigPipe
  , theProcConfigOutPipe = PipeReceiverConfigNewBuffer onErr
  , theProcConfigErrPipe = PipeReceiverConfigNewBuffer onErr
  }

-- | Setup a new asynchronous child 'Process', which will capture both 'stdout' and 'stderr' streams
-- into a given 'Buffer', and create a pipe for the 'stdin' stream to which input can be written
-- with 'processSend'. So 'theProcConfigInPipe' is set to 'PipeSenderConfigPipe', and both
-- 'theProcConfigOutPipe' and 'theProcConfigErrPipe' are set to 'PipeReceiverConfigBuffer' with the
-- same 'Buffer' for both.
asyncCapture
  :: ControlThreadErrorConfig
  -> ProcessProgramFile
  -> [Strict.Text]
  -> Table.Row Buffer
  -> ProcessConfig
asyncCapture onErr cmd args buffer =
  (asynchronous onErr cmd args)
  { theProcConfigOutPipe = PipeReceiverConfigBuffer buffer onErr
  , theProcConfigErrPipe = PipeReceiverConfigBuffer buffer onErr
  }

-- | Like 'asyncCapture', but automatically generates a new 'Buffer' for the 'ProcessConfig'. Also
-- note that this needs to be evaluated in a 'Manager' function context, so that a new 'Buffer' can
-- be created.
asyncCaptureNewBuffer
  :: ControlThreadErrorConfig
  -> ProcessProgramFile
  -> [Strict.Text]
  -> Manager (Table.Row Buffer, ProcessConfig)
asyncCaptureNewBuffer onErr cmd args = do
  buf <- newBuffer (Strict.unwords $ "ASYNC:" : (showAsText <$> (cmd : args)))
  pure (buf, asyncCapture onErr cmd args buf)

-- | Setup a new asynchronous child 'Process' which will use 'pipeFoldBufferLines' to evaluate an
-- 'EditStream' function.
asyncLineFilter
  :: ControlThreadErrorConfig
  -> ProcessProgramFile
  -> [Strict.Text]
  -> MVar fold
  -> (TextLine TextTags -> StateT fold IO ())
  -> ProcessConfig
asyncLineFilter onErr cmd args mvar filter =
  (asynchronous onErr cmd args)
  { theProcConfigOutPipe =
    PipeReceiverConfigFunction
    (Strict.unwords $ "FILTER:" : (showAsText <$> (cmd:args)))
    onErr
    ( pure $ \ pipeHandle ->
      fmap fst $
      readMVar mvar >>= \ fold ->
      newEditLineState Nothing >>=
      streamEditorStack
      (liftIO . (modifyMVar_ mvar . execStateT . filter))
      ( newEditStreamState fold >>=
        fmap fst .
        hFoldLines pipeHandle
        (const $ copyBufferClear >=> pushLine Before) >>=
        (throwError ||| pure)
      )
    )
  }

-- | Get the 'ProcessConfig' for a 'Process', use 'theProcConfigState' to get the 'ProcessState' for
-- the returned 'ProcessConfig'. You can call 'processStateInfo' on the returned 'ProcessConfig' to
-- obtain the 'ProcessStateInfo' status of the process: whether it has started running, whether it
-- is running, or whether it has halted.
processGetConfig :: MonadIO io => Table.Row Process -> io ProcessConfig
processGetConfig row = liftIO $
  let (Process mvar) = Table.theRowValue row in
  readMVar mvar

-- | Not for export. Operate on a process only if 'theProcConfigState' is 'ProcRunning', throw an
-- error otherwise.
processRequireRunning
  :: MonadIO io
  => Table.Row Process
  -> (ProcessState -> IO (Either Strict.Text a))
  -> io (Either Strict.Text a)
processRequireRunning row f = liftIO $
  let (Process mvar) = Table.theRowValue row in
  withMVar mvar $ \ cfg -> case theProcConfigState cfg of
    st@(ProcRunning{}) -> f st
    _ -> pure $ Left $ Strict.pack $ show (Table.theRowLabel row) <> " process not running"

-- | A stateful predicate that checks if a 'Process' is running or halted, returning 'True' if the
-- process is still running. Of course, a process may stop running the very moment after this
-- function returns 'True', and likewise a halted process may be re-launched by 'runProcess' the
-- very moment after this function returns 'False', so it is not entirely reliable in a system with
-- many activities beyond your control making changes to your process table. However, this function
-- can be useful from within an REPL, so it is provided.
processIsRunning :: MonadIO io => Table.Row Process -> io Bool
processIsRunning row =
  processRequireRunning row (const $ pure $ Right ()) >>=
  pure . (\ case { Left{} -> False; Right () -> True; })

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
    Nothing ->
      Left $
      Strict.pack $
      show (Table.theRowLabel row) <>
      " process not running"
    Just procHandle ->
      Right procHandle
  ) <$>
  getProcessExitCode procHandle

-- | Synchrnous wait on 'Process' completion, that is, freeze the current thread until the 'Process'
-- completes, then return the 'ProcessState' with the 'ExitCode'. Returns 'Nothing' if the 'Process'
-- has not started yet.
processWait :: MonadIO io => Table.Row Process -> io (Either Strict.Text ExitCode)
processWait row = processRequireRunning row $ fmap Right . waitForProcess . theProcStateProcHandle

-- | Remove a 'Job.Process' from the process table, but only if the process is not running. You must
-- evaluate 'processCancel' first if you want to forcibly halt the process. Usually a process
-- remains in the table after it halts so that you can gather it's output, and also gives you the
-- chance to re-run the process with the same arguments as before by using 'runProcess'.
processRemove :: Table.Row Process -> Manager Bool
processRemove row =
  editManagerEnvTable processTable $
  (\ case
    Table.Removed1{} -> True
    _ -> False
  ) <$>
  Table.update1
  (Table.byRowSelf row)
  ( fmap
    (\ running -> if running then Table.Keep else Table.Remove) .
    processIsRunning
  )

-- | Similar to 'processRemove', but removes all 'Job.Process's that are not running. from the
-- process table. Returns the number of elements removed from the table.
processRemoveAll :: Manager Int
processRemoveAll =
  editManagerEnvTable processTable $
  Table.remove $
  fmap not .
  processIsRunning

-- | Find a process by it's integer ID. When a 'Process' is created by 'newProcess', 'synchronous',
-- 'asynchronous', or any other such function, a reference to the process is stored in the current
-- process 'Manager's global state. You can list this 'Process' table with 'processList', and you
-- can use any of the ID numbers displayed to select a particular process using this function.
processFindById :: Int -> Manager (Maybe (Table.Row Process))
processFindById i = editManagerEnvTable processTable $ Table.select1 (Table.byRowId i)

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
  let (Process mvar) = Table.theRowValue row in
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader ManagerEnv)

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

-- | Send a signal to halt the 'Worker' thread.
haltWorker :: Worker -> IO ()
haltWorker = flip throwTo UserInterrupt  . theWorkerId

-- | not for export
--
-- Remove a worker from the 'globalBufferTable' regardless of it's current state (running or not).
clearWorker :: Table.Row Worker -> Manager Bool
clearWorker =
  editManagerEnvTable workerTable .
  fmap
  (\ case
    Table.Removed1{} -> True
    _ -> False
  ) .
  Table.remove1 .
  Table.byRowSelf

-- | Create a new data 'Buffer'. Thhis function also registers the buffer into the 'bufferTable' of
-- the 'ManagerEnv' execution environment, which is why it returns a 'Table.Row'. This is so if you
-- lose the handle to it while working in GHCI, you can get it back using 'getBuffer'.
newBuffer :: Table.Label -> Manager (Table.Row Buffer)
newBuffer lbl = managerState $ internalNewBuffer lbl Nothing Nothing

-- | Like 'newBuffer', but specify the buffer initial allocation size. This size may grow as needed,
-- but you may have a better idea of what initial size is best for a particular task.
newBufferSized :: Table.Label -> LineBufferSize -> CharBufferSize -> Manager (Table.Row Buffer)
newBufferSized lbl lineBufSiz charBufSiz = managerState $ internalNewBuffer lbl lineBufSiz charBufSiz

internalNewBuffer
  :: Table.Label
  -> LineBufferSize
  -> CharBufferSize
  -> StateT ManagerEnvState IO (Table.Row Buffer)
internalNewBuffer label lineBufSiz charBufSiz =
  Table.withinGroup bufferTable $
  ( liftIO $
    newEditTextState lineBufSiz charBufSiz >>= \ ed ->
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
withBuffer = withBuffer0 . Table.theRowValue

-- | Pretty-print each line of data in a 'Buffer'.
bufferShow :: MonadIO io => Table.Row Buffer -> TextRange LineIndex -> io (Either EditTextError ())
bufferShow row range =
  withBuffer row $
  foldLinesInRange range
  (\ _halt lineNum line -> liftIO $ do
     putStr (ralign6 lineNum)
     Strict.putStr ": "
     putStr (textLineString line)
     pure Nothing
  )
  ()

-- | Change the file path to which a buffer will be written when saved.
withBufferPath :: MonadIO io => Table.Row Buffer -> (Strict.Text -> Strict.Text) -> io ()
withBufferPath buf f = flip withBufferState (Table.theRowValue buf) $ bufStateFilePath %= f

-- | Read entire content of a 'Handle' into a 'Buffer'. The 'Label' here is for what the 'Worker'
-- should be called.
bufferHandle :: Table.Label -> Table.Row Buffer -> Handle -> Manager (Table.Row Worker)
bufferHandle label row handle =
  startWork label $ \ _worker ->
  withBuffer row $
  liftEditLine $
  newEditStreamState () >>=
  fmap fst . hFoldLines handle (\ _halt -> lineBreak Before) >>= \ case
    Left err -> liftIO $ hPutStrLn stderr $ show err
    Right () -> pure ()

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
  buffer <- newBuffer path
  worker <- bufferFileInto path buffer
  return (worker, buffer)

----------------------------------------------------------------------------------------------------

-- | When searching throught the global table, you can select items by one of 2 types of index
-- values: 1. their unique id, or 2. by their nickname. The 'getBuffer' and 'getWorker' functions
-- are polymorphic over the search index so you can use either with the same function.
class GlobalTableSearchIndex i where
  -- | Search for a 'BufferState' created by 'newBuffer'.
  getBuffer :: i -> Manager (Maybe (Table.Row Buffer))
  -- | Search for a 'ThreadTableElem' created by 'newThread'.
  getWorker :: i -> Manager (Maybe (Table.Row Worker))

instance GlobalTableSearchIndex Int where
  getBuffer index =
    hackEnvTableSelect1 bufferTable $
    Table.byRowId index
  getWorker index =
    hackEnvTableSelect1 workerTable $
    Table.byRowId index

instance GlobalTableSearchIndex (Strict.Text -> Bool) where
  getBuffer label =
    hackEnvTableSelect1 bufferTable $
    Table.byRowLabel label
  getWorker label =
    hackEnvTableSelect1 workerTable $
    Table.byRowLabel label

instance GlobalTableSearchIndex Strict.Text where
  getBuffer label =
    hackEnvTableSelect1 bufferTable $
    Table.byRowLabel (== label)
  getWorker label =
    hackEnvTableSelect1 workerTable $
    Table.byRowLabel (== label)
