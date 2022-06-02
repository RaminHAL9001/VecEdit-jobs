-- | This module provides all of the same APIs as the "Emacs.Manager" module, but with every function
-- in the 'Managerer' monad being lowered down into the @IO@ monad, which makes these functions easier
-- to use from within GHCI as you can evalute these functions without needing to wrap all of your
-- calls in the 'runManagerer' monad with a reference to a particular environment. Importing this
-- module creates a global environment so that all of the 'Manager' functions can run in @IO@.
module VecEdit.GHCI
  ( main,
    -- ** Data Buffers
    Manager.Buffer, Manager.bufferTable, newBuffer, listBuffers, bufferHandle, bufferFile,
    Manager.withBufferPath, Manager.showBuffer,
    -- ** Worker Threads
    Manager.Worker, Manager.workerTable, listWorkers, startWork, Manager.getWorkerStatus,
    -- ** Child Processes
    Manager.Process, Manager.processTable, listProcesses, runInBuffer, withBuffer,
    Manager.rerunProcess, Manager.pipeToBuffer, Manager.newReadPipeControl, Manager.sendToProc,
    -- ** Lifting 'Manager' functions
    ManagerEnv, Manager, ioManager,
    -- ** Testing
    redisplay, redisplayIOBuffer, debugViewBuffer,
    ------------------------
    testEditor, testByteStreamToLines, testGapBuffer, testTextEditor,
    ------------------------
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
  ( EditText, newEditTextState, evalEditText,
    lineNumber, flushLine, loadHandleEditText, cursorToEnd, insertString,
    foldLinesFromCursor, loadHandleEditText, debugViewTextEditor,
  )
import VecEdit.Text.LineBreak (allLineBreaks, allLineBreaks, lineBreak_LF)
import VecEdit.Text.String
  ( TextLine, hReadLines, isAsciiOnly, readLinesLiftGapBuffer,
    streamByteString, byteStreamToLines, cutUTF8TextLine,
  )
import VecEdit.Text.TokenizerTable (tokTableFold)
import VecEdit.Types
  ( VectorSize, RelativeDirection(..), Range(..), TextRange(..),
    EditTextError(..), ppGapBufferErrorInfo, 
  )
import qualified VecEdit.Vector.Editor as Vector
import VecEdit.Vector.Editor
  ( EditorMVectorType, EditorMVectorElem,
    liftEditor, sliceRange, currentBuffer,
  )
import VecEdit.Vector.Editor.GapBuffer
  ( newGapBufferState, evalGapBuffer,
    shiftCursor, pushItem, pullItem, popItem,
    gapBuffer3SliceInRange,
  )

import Control.Lens (use, (.=), (+=))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..))

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec

import System.IO (Handle, IOMode(ReadMode), openFile, hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess)

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
newBuffer :: Table.Label -> VectorSize -> IO (Table.Row Manager.Buffer)
newBuffer = fmap (fmap ioManager) Manager.newBuffer

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
listBuffers :: IO ()
listBuffers = ioManager Manager.listBuffers

-- | Print a list of all managed 'Worker' threads that are known to the 'ManagerEnv', meaning they were
-- created by the 'startWork' function. The 'ManagerEnv' execution context cannot track threads created
-- by 'forkIO' or 'forkOS'.
listWorkers :: IO ()
listWorkers = ioManager Manager.listWorkers

-- | Prints a list of all child 'Process's that are known to the 'ManagerEnv' created by
-- 'runInBuffer'. The 'ManagerEnv' execution context cannot track processes created by 'createProcess'
-- or any of its convenient functional wrappers.
listProcesses :: IO ()
listProcesses = ioManager Manager.listProcesses

-- | Run an external system process from a 'CreateProcess' spec, store it's output in the process
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
runInBuffer :: Table.Row Manager.Buffer -> CreateProcess -> IO (Table.Row Manager.Process)
runInBuffer = fmap (fmap ioManager) Manager.runInBuffer

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

-- | Run tests on the 'Buffer' data structure.
testEditor :: IO ()
testEditor = do
  -- test "sweepIOBuffer"
  edst <- Vector.newEditorState :: IO (Vector.EditorState MVector Strict.Text)
  flip Vector.evalEditor edst $ do
    let clean = do
          Vector.filterBuffer (pure . not . Strict.null) (\ () _ _ _ -> pure ()) () >>=
            Vector.withSubRange (Vector.fillWith "") . fst
          liftIO $ Strict.putStrLn "buffer cleaned"
    Vector.newCurrentBuffer 1
    Vector.putCurrentElem "zero"
    liftIO $ Strict.putStrLn "created buffer of 1 element"
    clean
    redisplay
    Vector.newCurrentBuffer 2
    Vector.putCurrentElem ""
    Vector.currentCursor += 1
    Vector.putCurrentElem "zero"
    liftIO $ Strict.putStrLn "created buffer of 2 elements"
    clean
    redisplay
    Vector.newCurrentBuffer 20
    Vector.currentCursor .= 0
    mapM_
      (\ e -> do
        Vector.putCurrentElem e
        Vector.currentCursor += 1
      )
      [ "zero", "one" , "two", "three"
      , "four", "five", "six", "seven"
      , ""
      , "eight", "nine", "ten", "eleven"
      , ""
      , "twelve", "thirteen"
      , ""
      , "fourteen", "fifteen", "sixteen"
      ]
    use Vector.currentCursor >>= \ i ->
      liftIO $ putStrLn $ "added " <> show i <> " elements"
    (found, ()) <- Vector.searchBuffer (pure . Strict.null) (\ () _ _ _ -> pure ()) ()
    liftIO $ Strict.putStrLn $ "Search for null element -> " <> Strict.pack (show found)
    redisplay
    liftIO $ Strict.putStrLn "--------------------"
    clean
    redisplay

-- | Run tests on the 'GapBuffer' data structure.
testGapBuffer :: IO ()
testGapBuffer = do
  buf <- newGapBufferState (Just "") 16
  let echo = liftIO . Strict.putStrLn
  let lbrk = liftIO $ Strict.putStrLn ""
  let redisplayln = redisplay >> lbrk
  let inline msg mvec = liftIO $ do
        let len = MVec.length mvec
        Strict.putStr msg
        if len == 0 then putStrLn ": [] (0)" else do
          Strict.putStr ": ["
          forM_ [0 .. len - 1] $ \ i ->
            MVec.read mvec i >>=
            Strict.putStr . Strict.pack . show >>
            Strict.putStr
            (if i < len - 1 then ", " else "] (" <> Strict.pack (show len) <> ")\n")
  let testRange a b = do
        let range = Range a b
        echo $ "select " <> Strict.pack (show range) <>
          " " <> Strict.pack (show (a, a+b-1))
        (lo, gap, hi) <- gapBuffer3SliceInRange range
        (imlo, imgap, imhi) <- liftIO $ (,,) <$> Vec.freeze lo <*> Vec.freeze gap <*> Vec.freeze hi
        slice <- liftEditor (sliceRange range <$> use currentBuffer) >>= liftIO . Vec.freeze
        if slice == imlo <> imgap <> imhi then echo "OK\n" else do
          echo "unexpected slices:"
          inline " lo" lo
          inline "gap" gap
          inline " hi" hi
          liftIO $ Strict.putStrLn $
            "expected: " <> Strict.pack (show slice) <>
            " (" <> Strict.pack (show $ Vec.length slice) <> ")"
          echo ""
  result <- flip evalGapBuffer buf $ do
    let test msg run = do
          e <- echo msg >> run
          redisplay
          echo $ "got item: " <> Strict.pack (show e <> "\n")
    mapM_ (pushItem Before) ["zero", "one", "two", "three", "four"]
    mapM_ (pushItem After) ["nine", "eight", "seven", "six", "five"]
    redisplayln
    test "pullItem Before" (pullItem Before)
    test "pullItem After"  (pullItem After)
    test "popItem Before"  (popItem Before)
    test "popItem After"   (popItem After)
    echo "shiftCursor -1" >> shiftCursor (-1) >> redisplayln
    echo "shiftCursor 1"  >> shiftCursor 1    >> redisplayln
    echo "shiftCursor -2" >> shiftCursor (-2) >> redisplayln
    echo "shiftCursor 2"  >> shiftCursor 2    >> redisplayln
    testRange  0 16
    testRange  1 15
    testRange  1 14
    testRange  2 11
    testRange  3 10
    testRange  3  9
    testRange  4  9
    testRange  4 10
    testRange  6  5
    testRange  0  4
    testRange  0  3
    testRange  1  3
    testRange  1  4
    testRange  1  6
    testRange 12  4
    testRange 11  5
    testRange 11  4
    testRange 10  3
  case result of
    Right{}  -> return ()
    Left err -> error $ Strict.unpack $ ppGapBufferErrorInfo err

-- | Used for testing and debugging 'Buffer's.
debugViewBuffer :: Table.Row Buffer -> IO (Either EditTextError ())
debugViewBuffer = flip withBuffer $ debugViewTextEditor

-- | Run tests on 'Buffer's.
testTextEditor :: IO (Table.Row Buffer)
testTextEditor = do
  buf <- newBuffer "test editor" 32
  result <-
    withBuffer buf $ do
      insertString $
        "<p>Emacs is a Lisp programming environment and app platform that is one\n" <>
        "of the most generally useful programming tools anyone could possibly ever\n" <>
        "use. The original Emacs software was first written at MIT back in 1976.\n" <>
        "It is well known among people in computer-related professions, and has a\n" <>
        "thriving community of users who contribute their own apps (which are\n" <>
        "called \"Major Modes\" and \"Minor Modes\"). This active community of users,\n" <>
        "along with a small team of and highly competent maintainers, keeps Emacs\n" <>
        "useful even in modern times.</p>\n\n" <>
        "<p>But due to how old the Emacs source code base is, the Lisp interpreter\n"
      let ins2 a b = insertString a >> insertString b
      ins2 "built-in to Emacs has a bit too much " "historical baggage. That is not to\n"
      ins2 "say that Emacs is a dinosaur. The code " "base (written in C) is actively\n"
      ins2 "maintained and has new feautres added " "regularly. Functionality for\n"
      ins2 "international font rendering, SVG graphics, " "and JSON parsing and data\n"
      ins2 "structure manipulation, are all built-" "in. Pretty soon, JIT compilation of\n"
      ins2 "Emacs Lisp code will be a standard feature, " "which will make Emacs Lisp\n"
      ins2 "code comparable in speed to lanugages " "like JavaScript.</p>"
      flushLine
      --debugViewTextEditor
      lineNumber
  case result of
    Right line ->
      Manager.showBuffer buf (TextRange 1 line) >>= \ case
        Right () -> return buf
        Left err -> error (show err)
    Left  err  -> error (show err)

----------------------------------------------------------------------------------------------------

-- | Run tests on 'TextString' functions.
testByteStreamToLines :: IO ()
testByteStreamToLines = do
  -- Show the content of the 'allLineBreaks' table.
  tokTableFold
    (\ before (c, tok, st) -> before >> print (c, show tok, st))
    (pure ())
    allLineBreaks
  putStrLn ""
  --
  -- Test 'byteStreamToLines' on an ByteString
  let mkStream = streamByteString $ UTF8.fromString $
        "this is a simple test\nto see what I can accomplish.\nIt is getting late\r\n" <>
        "I am having trouble focusing.\n\rI am not insomniac, just have lots to do.\v" <>
        "Here is hoping my tests all pass.\f" <>
        "zero\0one\0two\0three\0four\0five\0six\0seven\0eight\0nine\0" <>
        "The numbers are done for today.\n"
  let foldline _halt lbrk = do
        i <- get
        line <-
          isAsciiOnly >>=
          readLinesLiftGapBuffer .
          cutUTF8TextLine lbrk
        liftIO $ putStrLn $ show i <> ": " <> show (line :: TextLine ())
        put $! (i::Int) + 1
  let runLineBreaker table = do
        h <- mkStream
        buf <- newGapBufferState (Just 0) 128
        byteStreamToLines table buf h foldline (1::Int)
  putStrLn "test 'allLineBreaks'"
  runLineBreaker allLineBreaks
  putStrLn ""
  --
  putStrLn "test 'lineBreak_LF'"
  runLineBreaker lineBreak_LF
  putStrLn ""
  --
  -- Test 'byteStreamToLines' on a file via the 'hReadLines' function.
  buf <- newGapBufferState (Just 0) 128
  h <- openFile "./emacs-hacker.cabal" ReadMode
  flip (hReadLines buf h) (1::Int) $ \ _halt line -> do
    i <- get
    liftIO $ putStrLn $ show i <> ": " <> show (line :: TextLine ())
    put $! i + 1
  hClose h

----------------------------------------------------------------------------------------------------

-- | Run all tests.
main :: IO ()
main =
  newEditTextState 128 >>=
  ( evalEditText $
    liftIO (openFile "./emacs-hacker.cabal" ReadMode) >>=
    loadHandleEditText Nothing >>
    (do cursorToEnd Before
        flip (foldLinesFromCursor After) (1::Int) $ \ _halt i line -> do
          liftIO $ putStrLn $ show i <> ": " <> show (fmap (const ()) line)
          pure Nothing
    )
  ) >>= \ case
    Left err -> error $ show err
    Right _i -> pure ()
