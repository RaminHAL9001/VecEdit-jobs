-- | A job manager. A table of child processes is maintained, the
-- @STDIN@, @STDOUT@, and @STDERR@ of each process is captured in a
-- text buffer.
module VecEdit.Jobs
  ( Manager, ManagerEnv, newManagerEnv, runManager,
    ManagerEnvState, bufferTable, workerTable, processTable,
    Buffer, TextTags, newBuffer,
    bufferPrintAll, bufferList, bufferHandle, bufferFile, bufferFileInto, bufferShow,
    withBuffer, withBufferPath,
    Worker, WorkerStatus, startWork,
    workerPrintAll, workerList, workerGetStatus,
    Process, processWithBuffer, processNewBuffer, runProcess,
    processPrintAll, processList,
    processSend, processGetState, processWait, processCancel,
    ProcessLog(..), processGetLog,
    ReadPipeControl, newReadPipeControl, pipeToBuffer, editProcessBuffer,
    GlobalTableSearchIndex(..),
  ) where

import VecEdit.Types
  ( VectorSize, RelativeDirection(..), TextRange(..), LineIndex(..),
    GapBufferError(..), EditTextError(..),
  )

import VecEdit.Vector.Editor ( ralign6 )
import VecEdit.Vector.Editor.GapBuffer ( newGapBufferState, rethrowErrorInfo )
import VecEdit.Print.DisplayInfo (DisplayInfo(..), displayInfoShow, showAsText)
import VecEdit.Text.String
  ( TextLine, ReadLines, HaltReadLines, Word8GapBuffer, StringData,
    fromStringData, hReadLines, hReadLineBufferSize,
    readLinesLiftGapBuffer, convertString, theTextLineData,
  )
import VecEdit.Text.Editor
  ( EditText, EditTextState, loadHandleEditText, loadHandleEditText,
    newEditTextState, runEditText, pushLine, foldLinesInRange,
    maxLineIndex, validateBounds, mapRangeFreeze,
  )
import qualified VecEdit.Table as Table
import VecEdit.Table (Table)

import Control.Concurrent (ThreadId, forkIO, myThreadId, yield)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, newMVar, readMVar, modifyMVar, modifyMVar_, takeMVar, putMVar, withMVar)
import Control.Exception (SomeException, IOException, try, catch)
import Control.Lens (Lens', lens, use, (^.), (%=), (.=))
import Control.Monad -- re-exporting
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask, ReaderT(..))
import Control.Monad.State (StateT(..), runStateT, modify)
import Control.Monad.Trans (lift)

import Data.Char (isSpace)
import Data.Function (on)
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Vector as Vec
import Data.Vector (Vector)

import System.IO (Handle, IOMode(ReadMode), openFile, hClose, hFlush)
import System.Process
       ( ProcessHandle, CreateProcess, CmdSpec(..), Pid, StdStream(CreatePipe),
         getPid, createProcess, cmdspec, std_in, std_out,
         waitForProcess, interruptProcessGroupOf, getProcessExitCode,
       )
import System.Exit (ExitCode(..))

----------------------------------------------------------------------------------------------------

textShow :: Show a => a -> Strict.Text
textShow = Strict.pack . show

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
    { theProcSpec :: !CreateProcess
    , theProcState :: !ProcessState
    , theProcInPipe :: !(Maybe WritePipeControl)
    , theProcOutPipe :: !(Maybe (ReadPipeControl Int))
    , theProcErrPipe :: !(Maybe (ReadPipeControl Int))
    , theProcWaitThread :: !ThreadId
    , theProcStartTime :: !(Maybe UTCTime)
    , theProcEndTime :: !(Maybe UTCTime)
    , theProcCaptureBuffer :: !(Maybe Buffer)
    }

-- | Describes what state a 'Process' is in.
data ProcessState
  = ProcPending -- ^ Process is configured but hasn't been run yet.
  | ProcRunning
    { theProcStateProcHandle :: !ProcessHandle
    , theProcStatePid :: !Pid
    } -- ^ Process is running with this given handle.
  | ProcHalted
    { theProcStateExitCode :: !ExitCode
    } -- ^ Process halted with this exit code.

instance Show ProcessState where
  show = \ case
    ProcPending -> "ProcPending"
    ProcRunning{theProcStatePid=pid} -> "ProcRunning " <> show pid
    ProcHalted{theProcStateExitCode=exid} -> "ProcHalted " <> show exid

instance DisplayInfo ProcessConfig where
  displayInfo putStr cfg = do
    putStr $ Strict.pack $ show $ theProcState cfg
    putStr " "
    putStr $ Strict.pack $ show $ theProcSpec cfg

instance DisplayInfo Process where
  displayInfo putStr (Process mvar) = readMVar mvar >>= displayInfo putStr

instance DisplayInfo ProcessState where
  displayInfo putStr = \ case
    ProcPending -> putStr "(pending)\n"
    ProcRunning{theProcStatePid=pid} ->
      mapM_ putStr ["(running, pid=", showAsText pid, ")\n"]
    ProcHalted{theProcStateExitCode=exid} ->
      mapM_ putStr ["(halted, ", showAsText exid, ")\n"]

-- | An element of the 'ProcessState', if this is not 'Nothing' it indicates that 'theProcSpec' is
-- defined to use the 'CreatePipe' flag to either 'std_out' or 'std_err'. If a pipe is created, the
-- associated 'Handle' is passed to a function that runs in it's own thread to buffer all of the
-- characters read-from/written-to the 'Handle'. If the buffered characters are also stored into a
-- 'Buffer' 'thePipeBuffer' will be non-'Nothing' so you can retrieve the 'Buffer' to which this
-- process buffered lines.
data ReadPipeControl fold
  = ReadPipeControl
    { thePipeHandle :: !Handle
    , thePipeThread :: !ThreadId
    , thePipeBuffer :: !(Maybe Buffer)
    , thePipeWaitClose :: !(MVar (Either EditTextError (), fold))
    }

-- | Objects of this data type allow input to be written to the standard input of a child process.
newtype WritePipeControl = WritePipeControl Handle

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

-- | Given a child 'Process' value, evaluate an 'EditText' function on that buffer. The buffer may
-- not exist if the child process was created by 'Inherit'-ing the current process's output or error
-- stream, or if the a file stream was created instead to capture the child process output or error
-- streams. If for either of these reasons there is no process buffer, 'TextEditUndefined' is
-- returned and the 'EditText' function is not evaluated.
editProcessBuffer
  :: Table.Row Process
  -> EditText TextTags a
  -> IO (Either EditTextError a)
editProcessBuffer row f =
  let (Process mvar) = Table.theRowObject row in
  theProcCaptureBuffer <$> readMVar mvar >>= \ case
    Nothing  -> pure $ Left TextEditUndefined
    Just buf -> withBuffer0 buf f

writePipeControl :: Maybe Handle -> Maybe WritePipeControl
writePipeControl = fmap WritePipeControl

closeWritePipe :: MonadIO io => Maybe WritePipeControl -> io ()
closeWritePipe =
  liftIO .
  ( maybe (pure ()) $ \ (WritePipeControl handle) ->
    try (hClose handle) >>= \ case
      Left err -> seq (err :: IOException) $ pure ()
      Right () -> pure ()
  )

closeReadPipe :: MonadIO io => Maybe (ReadPipeControl fold) -> io ()
closeReadPipe = liftIO . maybe (pure ()) (hClose . thePipeHandle)

-- | Construct a 'PipeControl' for a particular 'Handle' that receives the output or error stream of
-- a child process. Pass a 'ReadLines' function to handle each line of input. If this 'ReadLines'
-- function throws an exception, the parent process handle is closed, which may or may not end the
-- child process. This function returns a new 'PipeControl' and an 'MVar' which will be filled when
-- the 'ReadLines' function halts. If you read this 'MVar' your thread will block until the child
-- process closes the pipe's file handle, which may not happen until the child process halts.
newReadPipeControl
  :: MonadIO io
  => Maybe Handle
  -> (HaltReadLines fold void -> TextLine tags -> ReadLines fold ())
  -> fold
  -> io (Maybe (ReadPipeControl fold))
newReadPipeControl stream useLine fold = liftIO $ case stream of
  Nothing -> pure Nothing
  Just stream -> do
    foldMVar <- newEmptyMVar
    lineBuffer <- newGapBufferState Nothing hReadLineBufferSize  
    thid <- forkIO $ do
      result <- hReadLines lineBuffer stream useLine fold
      hClose stream
      putMVar foldMVar result
    pure $ Just $
      ReadPipeControl
      { thePipeHandle = stream
      , thePipeThread = thid
      , thePipeBuffer = Nothing
      , thePipeWaitClose = foldMVar
      }

-- | Like 'newReadPipeControl' but simply writes all lines into the given 'Buffer'. The number of
-- lines read are sent to the 'MVar'.
pipeToBuffer
  :: MonadIO io
  => Maybe Handle
  -> RelativeDirection -- ^ incoming lines can be buffered in reverse order
  -> Buffer
  -> io (Maybe (ReadPipeControl Int))
pipeToBuffer stream rel buffer =
  newReadPipeControl stream
  ( const $
    liftIO . withBuffer0 buffer . pushLine rel >=> \ case
      Left err -> readLinesLiftGapBuffer $ errorEditTextToReadLine err
      Right () -> modify (+ 1)
  ) 0

errorEditTextToReadLine :: EditTextError -> Word8GapBuffer void
errorEditTextToReadLine = \ case
  EditTextError  err -> rethrowErrorInfo err
  EditLineError  err -> rethrowErrorInfo err
  TextEditUndefined  -> throwError $ GapBufferFail "(evaluated to 'empty')"
  EditTextFailed msg -> throwError $ GapBufferFail msg
  EditLineIndexError i lo hi ->
    throwError $
    GapBufferFail $
    "(index " <> textShow i <>
    " out of bounds, limits are " <> textShow lo <> ".." <> textShow hi <> ")"
  EditCharIndexError line i lo hi ->
    throwError $
    GapBufferFail $
    "(on line " <> textShow line <>
    ": char index " <> textShow i <>
    " out of bounds, limits are " <> textShow lo <> ".." <> textShow hi <> ")"

labelCreateProcess :: CreateProcess -> Strict.Text
labelCreateProcess mkproc =
  case cmdspec mkproc of
    ShellCommand path -> Strict.pack $ takeWhile (not . isSpace) $ dropWhile isSpace path
    RawCommand path args -> Strict.unwords $ Strict.pack <$> (path : args)

runProcess0 :: MonadIO io => Process -> io ()
runProcess0 (Process mvar) =
  liftIO $
  modifyMVar_ mvar $ \ cfg -> do
    (pin, pout, perr, phandle) <- createProcess $ theProcSpec cfg
    pid <- getPid phandle
    case pid of
      Nothing -> do
        this <- myThreadId
        t0 <- getCurrentTime
        exitCode <- waitForProcess phandle
        t1 <- getCurrentTime
        pure cfg
          { theProcState = ProcHalted exitCode
          , theProcInPipe = Nothing
          , theProcOutPipe = Nothing
          , theProcErrPipe = Nothing
          , theProcWaitThread = this
          , theProcStartTime = Just t0
          , theProcEndTime = Just t1
          }
      Just pid -> do
        let inPipe = writePipeControl pin
        let capture pipe =
              maybe
              (pure Nothing)
              (pipeToBuffer pipe Before)
              (theProcCaptureBuffer cfg)
        errPipe <- capture perr
        outPipe <- capture pout
        waitThread <- forkIO $ do
          exitCode <- waitForProcess phandle
          t1 <- getCurrentTime
          closeWritePipe inPipe
          closeReadPipe outPipe
          closeReadPipe errPipe
          modifyMVar_ mvar $ \ cfg -> pure $ cfg
            { theProcState = ProcHalted exitCode
            , theProcEndTime = Just t1
            }
        t0 <- getCurrentTime
        pure cfg
          { theProcState =
            ProcRunning
            { theProcStateProcHandle = phandle
            , theProcStatePid = pid
            }
          , theProcInPipe = inPipe
          , theProcOutPipe = outPipe
          , theProcErrPipe = errPipe
          , theProcWaitThread = waitThread
          , theProcStartTime = Just t0
          , theProcEndTime = Nothing
          }

-- | Run a particular 'Process' that has been created by 'processNewBuffer' or
-- 'processWithBuffer'. The 'Table.Row' entry is reused, so it will have the same row ID and label
-- as before, and will remain in the process table in the same position as before.
runProcess :: MonadIO io => Table.Row Process -> io ()
runProcess = void . runProcess0 . Table.theRowObject

-- | not for export
--
-- Create the meta-information around a 'CreateProcess' specification that will be used by the
-- mechanisms in this module for controlling child processes.
newProcessConfig :: MonadIO io => CreateProcess -> io ProcessConfig
newProcessConfig exe = liftIO $ do
  thid <- myThreadId
  pure ProcessConfig
    { theProcSpec = exe
    , theProcState = ProcPending
    , theProcInPipe = Nothing
    , theProcOutPipe = Nothing
    , theProcErrPipe = Nothing
    , theProcWaitThread = thid
    , theProcStartTime = Nothing
    , theProcEndTime = Nothing
    , theProcCaptureBuffer = Nothing
    }

-- | Given a 'CreateProcess' spec, create an external system 'Process' to be run by 'runProcess',
-- and prepare to store it's output in the process table. The 'Process' will not actually be
-- executed until 'runProcess' is called on the 'Process' handle created by this function.
--
-- If you configure an input pipe, you can dump strings to the process's standard input stream using
-- the 'processSend' function. All output from the process created if buffered in the given
-- 'Buffer'. Note that the 'CreateProcess' 'std_out' and 'std_in' fields are forced to 'CreatePipe'
-- regardless of it's value when passed to this function, this is to force the capture of the
-- process output into the given 'Buffer', and to allow input to be sent to the process, since it
-- will run asynchronously.
--
-- This function returns a 'Table.Row' because, like with 'Buffer's, any new process created has a
-- handle for it stored in a table in the 'Manager' monad execution environment so it can be retrieved
-- at any time.
processWithBuffer :: Table.Row Buffer -> CreateProcess -> Manager (Table.Row Process)
processWithBuffer buffer exe =
  editManagerEnvTable processTable $ do
    mvar <- liftIO $ do
      cfg <- newProcessConfig (exe{ std_in = CreatePipe, std_out = CreatePipe })
      newMVar (cfg{ theProcCaptureBuffer = Just $ Table.theRowObject buffer })
    Table.insert (labelCreateProcess exe) $ Process mvar

-- | Like 'processWithBuffer', but automatically creates a new 'Buffer' to capture STDOUT (and
-- STDERR is always captured, regardless of which method starts it). The 'Process' and 'Buffer'
-- handles are both returned.
processNewBuffer :: CreateProcess -> Manager (Table.Row Buffer, Table.Row Process)
processNewBuffer exe = do
  buffer <- newBuffer (Strict.pack $ "STDOUT: " <> show (cmdspec exe)) 63
  process <- processWithBuffer buffer exe
  pure (buffer, process)

-- | If the 'Process' was created with the 'std_in' field of the 'CreateProcess' spec set to
-- 'CreatePipe', a 'Handle' is available for sending input to the Process (unless the 'Process' has
-- recently closed this handle). This function allows you to send a string of input to the
-- 'Process'.
processSend :: MonadIO io => Table.Row Process -> StringData -> io ()
processSend row str = liftIO $
  let (Process mvar) = Table.theRowObject row in
  withMVar mvar $ \ cfg -> case theProcInPipe cfg of
    Nothing -> fail $ show (Table.theRowLabel row) <> " process created without input pipe"
    Just (WritePipeControl handle) -> case fromStringData str of
      Nothing -> pure ()
      Just txt -> Strict.hPutStr handle txt >> hFlush handle

-- | Get the 'ProcessState' for a 'Process'.
processGetState :: MonadIO io => Table.Row Process -> io ProcessState
processGetState row = liftIO $
  let (Process mvar) = Table.theRowObject row in
  theProcState <$> readMVar mvar

-- | Send a signal to the given 'Process' handle forcing it to halt. This uses
-- 'interruptProcessGroupOf', which sends the POSIX signal @SIGINT@ ("interrupt") to the operating
-- system process.
processCancel :: MonadIO io => Table.Row Process -> io (Maybe ExitCode)
processCancel = processGetState >=> liftIO . \ case
  ProcPending -> pure Nothing
  ProcRunning{theProcStateProcHandle=procHandle} -> do
    interruptProcessGroupOf procHandle
    yield
    getProcessExitCode procHandle
  ProcHalted{theProcStateExitCode=code} -> pure $ Just code

-- | Synchrnous wait on 'Process' completion, that is, freeze the current thread until the 'Process'
-- completes, then return the 'ProcessState' with the 'ExitCode'. Returns 'Nothing' if the 'Process'
-- has not started yet.
processWait :: MonadIO io => Table.Row Process -> io (Maybe ExitCode)
processWait = processGetState >=> liftIO . \ case
  ProcPending -> pure Nothing
  ProcRunning{ theProcStateProcHandle=procHandle } -> Just <$> waitForProcess procHandle
  ProcHalted{ theProcStateExitCode=code } -> pure $ Just code

-- | Obtain lines of text from the 'ProcessSTDOUT' or 'ProcessSTDERR' of a 'Process' that have been
-- cached in a buffer.
processGetLog
  :: MonadIO m
  => Table.Row Process
  -> LineIndex
  -> Maybe LineIndex
  -> m ProcessLog
processGetLog row fromLine toLine =
  liftIO $
  fmap
  (\ case
    Left err -> ProcessTextError err
    Right ok -> ok
  ) $
  editProcessBuffer row $
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
newBuffer label initSize =
  editManagerEnvTable bufferTable $
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
