-- | This module provides all of the same APIs as the "Emacs.Manager" module, but with every function
-- in the 'Managerer' monad being lowered down into the @IO@ monad, which makes these functions easier
-- to use from within GHCI as you can evalute these functions without needing to wrap all of your
-- calls in the 'runManagerer' monad with a reference to a particular environment. Importing this
-- module creates a global environment so that all of the 'Manager' functions can run in @IO@.
module VecEdit.GHCI
  ( -- ** Data Buffers
    Manager.Buffer, Manager.bufferTable, newBuffer, bufferPrintAll, bufferHandle, bufferFile,
    Manager.withBufferPath, Manager.bufferShow, withBuffer,
    -- ** Worker Threads
    Manager.Worker, Manager.workerTable, workerPrintAll, startWork, Manager.workerGetStatus,
    -- ** Child Processes
    Manager.Process, Manager.processTable, processPrintAll, newProcess, runProcess,
    Manager.synchronous, Manager.asynchronous, Manager.asyncCapture,
    Manager.asyncCaptureNewBuffer,
    Manager.processSend, Manager.processWait, Manager.processGetLog,
    -- ** Lifting 'Manager' functions
    ManagerEnv, Manager, ioManager,
    -- ** Debugging
    debugViewBuffer, redisplay, redisplayFunction, redisplayIOBuffer,
    -- ** Re-exports
    Manager.GlobalTableSearchIndex(..),
    module Control.Monad,
    module VecEdit.Text.Editor,
    module VecEdit.Text.String,
  ) where

import qualified VecEdit.Jobs as Manager
import qualified VecEdit.Table as Table
import VecEdit.Jobs
  ( Manager, ManagerEnv, newManagerEnv, runManager,
    Buffer,
  )
import VecEdit.Text.Editor
  ( EditText, newEditTextState, evalEditText, lineNumber,
    cursorToEnd,
    foldLinesFromCursor, debugViewTextEditor,
  )
import VecEdit.Text.String
  ( TextLine, hFoldLines, streamByteString,
    LineEditor
      ( liftEditLine, countLines, pushLine, popLine, cutLine, copyBuffer,
        shiftCursor, pushChar, popChar, tryPopChar, countChars
      ),
    insertString, streamFoldLines, editLineTokenizer,
    EditLineResult(..), newEditLineState, onEditLineState, runEditLine
  )
import VecEdit.Types (EditTextError(..))
import qualified VecEdit.Vector.Editor as Vector
import VecEdit.Vector.Editor (EditorMVectorType, EditorMVectorElem)

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..))

import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import Data.Vector.Mutable (MVector, IOVector)

import System.IO (Handle)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Exec

----------------------------------------------------------------------------------------------------

globalManagerEnv :: ManagerEnv
globalManagerEnv = unsafePerformIO newManagerEnv
{-# NOINLINE globalManagerEnv #-}

-- | This function lowers (opposite of 'lift') any 'Manager' type of function down to an @IO@ type of
-- function, the trade-off is that you always end up using the global environment variable bound in
-- this module.
ioManager :: Manager a -> IO a
ioManager = flip runManager globalManagerEnv

-- | Create a new buffer, and this also registers the buffer into the global buffer table so if you
-- lose the handle to it while working in GHCI, you can get it back using 'getBuffer'.
newBuffer :: Table.Label -> IO (Table.Row Manager.Buffer)
newBuffer = ioManager . Manager.newBuffer

-- | Manipulate a buffer if you have it's handle already.
withBuffer :: Table.Row Manager.Buffer -> EditText Manager.TextTags a -> IO (Either EditTextError a)
withBuffer = fmap (fmap ioManager) Manager.withBuffer

-- | Read entire content of a 'Handle' into a 'Buffer'. The 'Label' here is for what the 'Worker'
-- should be called.
bufferHandle :: Table.Label -> Table.Row Manager.Buffer -> Handle -> IO (Table.Row Manager.Worker)
bufferHandle = fmap (fmap (fmap ioManager)) Manager.bufferHandle

-- | Load a file from the given path, return a reference to the 'Buffer' and the 'Worker' that was
-- used to load the buffer.
bufferFile :: Strict.Text -> IO (Table.Row Manager.Worker, Table.Row Manager.Buffer)
bufferFile = fmap ioManager Manager.bufferFile

-- | Create a new 'Worker', passing a 'Label' to identify the worker in the 'listWorkers' table, and
-- a task of type @IO ()@ to perform.
--
-- This function returns a 'Table.Row' because, like with 'Buffer's, any new 'Worker' thread created
-- has a handle for it stored in a table in the 'Manager' monad execution environment so it can be
-- retrieved at any time.
startWork
  :: Table.Label
  -> (Table.Row Manager.Worker -> IO (Either EditTextError ()))
  -> IO (Table.Row Manager.Worker)
startWork = fmap (fmap ioManager) Manager.startWork

-- | Print a list of all data 'Buffer's in the 'ManagerEnv' that were created by 'newBuffer'.
bufferPrintAll :: IO ()
bufferPrintAll = ioManager Manager.bufferPrintAll

-- | Print a list of all managed 'Worker' threads that are known to the 'ManagerEnv', meaning they were
-- created by the 'startWork' function. The 'ManagerEnv' execution context cannot track threads created
-- by 'forkIO' or 'forkOS'.
workerPrintAll :: IO ()
workerPrintAll = ioManager Manager.workerPrintAll

-- | Prints a list of all child 'Process's that are known to the 'ManagerEnv' created by
-- 'runInBuffer'. The 'ManagerEnv' execution context cannot track processes created by 'createProcess'
-- or any of its convenient functional wrappers.
processPrintAll :: IO ()
processPrintAll = ioManager Manager.processPrintAll

-- | Define an external system process that can be run with 'runProcess', store it into the process
-- table. If you configure an input pipe, you can dump strings to the process's standard input
-- stream using the 'sendToProc' function. All output from the process created if buffered in the
-- given 'Buffer'. Note that the 'CreateProcess' 'std_out' and 'std_in' fields are forced to
-- 'Inherit' regardless of it's value when passed to this function, this is to force the capture of
-- the process output into the given 'Buffer', and to allow input to be sent to the process, since
-- it will run asynchronously.
--
-- This function returns a 'Table.Row' because, like with 'Buffer's, any new process created has a
-- handle for it stored in a table in the 'Manager' monad execution environment so it can be retrieved
-- at any time.
newProcess :: Manager.ProcessConfig -> IO (Table.Row Manager.Process)
newProcess = ioManager . Manager.newProcess

-- | After creating a new 'Process' with 'newProcess', you can ask the operating system to actually
-- run the process with this function, which starts a job.
runProcess :: Table.Row Manager.Process -> IO (Either Strict.Text Exec.ProcessHandle)
runProcess = ioManager . Manager.runProcess

----------------------------------------------------------------------------------------------------

redisplayFunction
  :: MonadIO editor
  => ((Strict.Text -> editor Bool)
      -> (Strict.Text -> Strict.Text -> editor ())
      -> editor ()
      -> editor ()
     )
  -> editor ()
redisplayFunction f = f
  (pure . not . Strict.null)
  (\ prefix elem -> liftIO $ Strict.putStrLn $ prefix <> Strict.pack (show elem))
  (liftIO $ putStrLn "....")

-- | Used for testing 'Buffer's, shows the low-level mutable vector conent of an editable vector of
-- 'Strict.Text'.
redisplay
  :: ( MonadIO editor, MonadState st editor, Vector.HasEditorState st
     , EditorMVectorType st ~ MVector
     , EditorMVectorElem st ~ Strict.Text
     )
  => editor ()
redisplay = redisplayFunction Vector.printBuffer

-- | Used for testing 'Buffer's, pretty-prints the content of a mutable 'IOVector' of 'Strict.Text'
-- elements.
redisplayIOBuffer :: MonadIO editor => IOVector Strict.Text -> editor ()
redisplayIOBuffer mvec = redisplayFunction (\ a b c -> Vector.printOverIOBuffer a b c mvec)

-- | Used for testing and debugging 'Buffer's.
debugViewBuffer :: Table.Row Buffer -> IO (Either EditTextError ())
debugViewBuffer = flip withBuffer $ debugViewTextEditor
  
