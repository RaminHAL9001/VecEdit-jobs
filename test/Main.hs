module Main where

import VecEdit.Types
  ( RelativeDirection(..), Range(..), TextRange(..),
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
import VecEdit.Text.String
  ( TextLine, hReadLines, isAsciiOnly, readLinesLiftGapBuffer,
    streamByteString, byteStreamToLines, cutUTF8TextLine,
  )
import VecEdit.Text.Editor
  ( newEditTextState, evalEditText,
    lineNumber, flushLine, loadHandleEditText, cursorToEnd, insertString,
    foldLinesFromCursor, loadHandleEditText, debugViewTextEditor,
  )
import VecEdit.Jobs (Manager, Buffer, withBuffer, newBuffer, showBuffer)
import qualified VecEdit.Table as Table

import VecEdit.Text.TokenizerTable (tokTableFold)
import VecEdit.Text.LineBreak (allLineBreaks, allLineBreaks, lineBreak_LF)

import Control.Lens (use, (.=), (+=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState(..))

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Vector as Vec
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict

import System.IO (IOMode(ReadMode), openFile, hClose)

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
testTextEditor :: Manager (Table.Row Buffer)
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
      showBuffer buf (TextRange 1 line) >>= \ case
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
