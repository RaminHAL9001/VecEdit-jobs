module Main where

import qualified VecEdit.Jobs as Manager
import VecEdit.Types
  ( RelativeDirection(..), Range(..), Boundary(..),
    EditTextError(..), ppGapBufferErrorInfo,
  )

import qualified VecEdit.Vector.Editor as Vector
import VecEdit.Vector.Editor
  ( EditorMVectorType, EditorMVectorElem,
    liftEditor, sliceRange, currentBuffer,
  )
import VecEdit.Vector.Editor.GapBuffer
  ( newGapBufferState, evalGapBuffer, gapBuffer3SliceInRange,
  )
import qualified VecEdit.Vector.Editor.GapBuffer as GapBuf
import VecEdit.Text.Line
  ( StringData, CharVector, toStringData, convertString,
    toCharStream, charStreamEnd,
  )
import VecEdit.Text.Line.Editor
  ( LineEditor(pushLine, copyBuffer), copyBufferClear, liftEditLine,
    insertString, editLineTokenizer, onEditLineState,
  )
import VecEdit.Text.Stream
  ( newEditStreamState, streamByteString, streamFoldLines,
  )
import VecEdit.Text.UTF8
  ( UTF8Decoder(..), utf8Decoder, utf8DecoderPushByte,
  )
import VecEdit.Text.Editor
  ( lineNumber, cursorToEnd, loadStreamEditText,
    foldLinesFromCursor, debugViewTextEditor,
  )
import VecEdit.Jobs
  ( Manager, Buffer, newManagerEnv, runManager, withBuffer, newBuffer,
  )
import qualified VecEdit.Table as Table

import VecEdit.Print.DisplayInfo (DisplayInfo(..), displayInfoPrint, displayInfoShow, ralign6)
import VecEdit.Text.Line.Break (allLineBreaks, allLineBreaks, lineBreak_LF)
import VecEdit.Text.Parser
       ( StringParser, StringParserState(..), StringParserResult(ParseOK, ParseWait),
         runStringParser, stringParserState, feedStringParser,
       )
import VecEdit.Text.TokenizerTable (tokTableFold)

import Control.Applicative ((<|>), some)
import Control.Exception (catch, IOException)
import Control.Lens (use, (.~), (.=), (+=))
import Control.Monad -- re-exporting
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State.Class (MonadState(..), modify)
import Control.Monad.State (StateT, State, runState, evalStateT)

import Data.Bits (Bits(bitSizeMaybe,testBit))
import Data.Char (chr, ord, isAscii, isPrint, isLower, toLower, toUpper)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as Bytes
import Data.Char (isSpace, isDigit)
import qualified Data.Set as Set
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict
import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVec
import Data.Word (Word32)

import Text.Parser.Char (satisfy, char, spaces, oneOf, anyChar)
import Text.Parser.Combinators (many, manyTill, choice, (<?>))

import System.IO (Handle, IOMode(ReadMode), openFile, hClose)

--import Debug.Trace (trace, traceM)

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
    let clean =
          Vector.updateBuffer
          (\ _i line () ->
            pure $ flip (,) () $
            if Strict.null line then Vector.ItemRemove else Vector.ItemKeep
          )
          () >>=
          Vector.withSubRange (Vector.fillWith "") . fst >>
          liftIO (Strict.putStrLn "buffer cleaned")
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
    mapM_ (GapBuf.pushItem Before) ["zero", "one", "two", "three", "four"]
    mapM_ (GapBuf.pushItem After) ["nine", "eight", "seven", "six", "five"]
    redisplayln
    test "pullItem Before" (GapBuf.pullItem Before)
    test "pullItem After"  (GapBuf.pullItem After)
    test "popItem Before"  (GapBuf.popItem Before)
    test "popItem After"   (GapBuf.popItem After)
    echo "shiftCursor -1" >> GapBuf.shiftCursor (-1) >> redisplayln
    echo "shiftCursor 1"  >> GapBuf.shiftCursor 1    >> redisplayln
    echo "shiftCursor -2" >> GapBuf.shiftCursor (-2) >> redisplayln
    echo "shiftCursor 2"  >> GapBuf.shiftCursor 2    >> redisplayln
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
  liftIO $ putStrLn "new buffer: \"test editor\""
  buf <- newBuffer "test editor"
  liftIO $ putStrLn "\"test editor\" insertString ..."
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
      copyBuffer
      --debugViewTextEditor
      lineNumber
  liftIO $ putStrLn "\"test editor\" bufferShow ..."
  case result of
    Right line ->
      Manager.bufferShow buf (Boundary 1 line) >>= \ case
        Right () -> return buf
        Left err -> error (show err)
    Left  err  -> error (show err)

----------------------------------------------------------------------------------------------------

showBinary :: Bits b => b -> String
showBinary w = loop (0::Int) "" where
  len = maybe maxBound id $ bitSizeMaybe w
  loop cursor stack =
    if cursor >= len then stack else
    (loop $! cursor + 1) $
    ( if mod cursor 8 == 7 then (' ' :) else
      if mod cursor 4 == 3 then (',' :) else id
    ) $
    (if testBit w cursor then '1' else '0') : stack

printBinary :: Bits b => b -> IO ()
printBinary = putStrLn . showBinary

showUTF8Decoder :: UTF8Decoder -> String
showUTF8Decoder o = unlines
  [ "    decoded: " <> showBinary (theUTF8DecodedValue o)
  , "      store: " <> showBinary (theUTF8ElemStore o)
  , "elemCount: " <> show (theUTF8ElemCount o) <>
    ", countdown: " <> show (theUTF8Countdown o)
  , "total chars: " <> show (theUTF8CharCounter o) <>
    ", total bytes: " <> show (theUTF8ByteCounter o)
  ]

-- | Run a 'UTF8Decoder' on a given 'UTF8.ByteString', printing to STDOUT the state of the decoder
-- after every byte. Optionally pass a 'String' equivalent of the encoded 'UTF8.ByteString' to
-- compare the result of the 'UTF8Decoder', it will fail if the decoded characters are not identical
-- to the given 'String'.
illustrateUTF8Decoder :: Maybe String -> UTF8.ByteString -> IO ()
illustrateUTF8Decoder compare = flip evalStateT compare . loop utf8Decoder where
  word32 :: Char -> Word32
  word32 = fromIntegral . ord
  stepCompare c = get >>= \ case
    Nothing -> pure True
    Just "" -> liftIO $ do
      putStrLn "*** ERROR: expected string ends prematurely ***"
      pure False
    Just (s:str) ->
      if c == s then put (Just str) >> pure True else liftIO $ do
        putStrLn $
          "*** ERROR: decoded character " <> show c <>
          " does not match expected " <> show s <>
          " ***\n decoded: " <> showBinary (word32 c) <>
          "\nexpected: " <> showBinary (word32 s)
        pure False
  checkFinal = get >>= \ case
    Nothing -> pure ()
    Just "" -> pure ()
    Just str ->
      liftIO $ putStrLn $
      "*** ERROR: decoding string ended prematurely ***\nremaining: " <> show str
  showState :: StateT (Maybe String) IO a -> State UTF8Decoder (StateT (Maybe String) IO a)
  showState ret =
    get >>= \ st -> pure $
    liftIO (putStrLn $ showUTF8Decoder st) >> ret
  loop :: UTF8Decoder -> UTF8.ByteString -> StateT (Maybe String) IO ()
  loop decoder0 str0 =
    if Bytes.length str0 <= 0 then checkFinal else do
      let c8 = Bytes.head str0
      let c = chr $ fromIntegral c8
      let str = Bytes.tail str0
      let (more, decoder) =
            flip runState decoder0 $
            utf8DecoderPushByte True
            (showState $ pure True)
            (\ c -> showState $ do
                liftIO $ putStrLn $ "*** EMIT CHAR: " <> show c <> " ***\n"
                stepCompare c
            )
            ( showState $ do
                liftIO $ putStrLn $ "*** ERROR: failed to decode ***"
                pure False
            )
            c8
      liftIO $ putStrLn $ "*** INPUT WORD " <> showBinary c8 <>
        (if isAscii c && isPrint c then (' ' : show c) else "") <>
        " ***"
      ok <- more
      when ok $ loop decoder str

----------------------------------------------------------------------------------------------------

haiku :: String
haiku = 
  "菜の花や\n" <>
  "月は東に\n" <>
  "日は西に\n"

haikuUTF8 :: Bytes.ByteString
haikuUTF8 = UTF8.fromString haiku

-- | Some examples of poorly encoded UTF8 strings, each with 1 too many component bytes.
tooManyBytesUTF8 :: [UTF8.ByteString]
tooManyBytesUTF8 =
  [ "\xDF\x81\x80 Hello, world!"
  , "\xEF\x81\x81\x80 Hello, world!"
  , "\xF7\x81\x81\x81\x80 Hello, world!"
  ]

-- | Some examples of poorly encoded UTF8 strings, each with 1 too few component bytes.
tooFewBytesUTF8 :: [UTF8.ByteString]
tooFewBytesUTF8 =
  [ "\xDF Hello, world!"
  , "\xEF\x81 Hello, world!"
  , "\xF7\x81\x81 Hello, world!"
  ]

-- | Some examples of poorly encoded UTF8 strings, these are multi-byte UTF-8 encoded characters
-- with code points smaller than 128 (ASCII characters). These characters should be encoded as
-- single-bytes. All of these strings are incorrect ways of encoding the capital letter 'A'.
multiByteASCII :: [UTF8.ByteString]
multiByteASCII =
  [ "\xC1\x81 Hello, world!"
  , "\xE0\x81\x81 Hello, world!"
  , "\xF0\x80\x81\x81 Hello, world!"
  ]

sampleText :: String
sampleText =
  "this is a simple test\nto see what I can accomplish.\nIt is getting late\r\n" <>
  "I am having trouble focusing.\n\rI am not insomniac, just have lots to do.\v" <>
  "Here is hoping my tests all pass.\f" <>
  "zero\0one\0two\0three\0four\0five\0six\0seven\0eight\0nine\0" <>
  "The numbers are done for today.\n"

sampleTextUTF8 :: UTF8.ByteString
sampleTextUTF8 =  UTF8.fromString sampleText

-- | Run tests on 'TextString' functions.
testByteStreamToLines :: Manager ()
testByteStreamToLines = do
  -- Show the content of the 'allLineBreaks' table.
  liftIO $ do
    tokTableFold
      (\ before (c, tok, st) -> before >> print (c, show tok, st))
      (pure ())
      allLineBreaks
    putStrLn ""
  --
  let mkStream = streamByteString $ sampleTextUTF8 <> haikuUTF8
  --
  -- Test 'byteStreamToLines' on an ByteString
  buf <- newBuffer "test bytes stream to lines"
  let runLineBreaker lbrkTable h = withBuffer buf $ liftEditLine $ do
        -- Evaluate a 'streamFoldLines' function to produce a bunch of 'TextLine's from the 'h'
        -- stream. Then evaluate this in the 'foldEditLinesIO' function to evaluate the monad.
        onEditLineState $ editLineTokenizer .= lbrkTable
        (result, _readst) <-
          newEditStreamState (0::Int) >>=
          streamFoldLines h
          ( \ _halt lbrk -> do
              i <- modify (+ 1) >> get
              line <- copyBufferClear lbrk
              liftIO $ putStrLn $ show i <> ": " <> show line
              pushLine Before line
          )
        liftIO $ putStrLn $ case result of
          Left err -> show err
          Right  i -> "Lines: " <> show i
  liftIO $ putStrLn "test 'allLineBreaks'"
  liftIO mkStream >>=
    runLineBreaker allLineBreaks
  liftIO $ putStrLn ""
  --
  liftIO $ putStrLn "test 'lineBreak_LF'"
  liftIO mkStream >>=
    runLineBreaker lineBreak_LF
  liftIO $ putStrLn ""
  --

catchIOException :: IO a -> (IOException -> IO a) -> IO a
catchIOException = catch

tryOpenFile :: FilePath -> IO (FilePath, Handle) -> IO (FilePath, Handle)
tryOpenFile path tryNext =
  ((,) path <$> openFile path ReadMode)
  `catchIOException`
  const tryNext

-- | This is a test function.
bufferCabalFile :: Manager (Table.Row Buffer)
bufferCabalFile = do
  let filepath1 = "./VecEdit-jobs/VecEdit-jobs.cabal" :: String
  let filepath2 = "./VecEdit-jobs.cabal" :: String
  let filepath3 = "./README.md" :: String
  let showLinesNumbered _halt i line = do
        liftIO $ putStrLn $ show i <> ": " <> show (fmap (const ()) line)
        pure Nothing
  buf <- newBuffer "test load file from handle"
  result <- withBuffer buf $ do
    (path, h) <- liftIO $
      tryOpenFile filepath1 $
      tryOpenFile filepath2 $
      tryOpenFile filepath3 $
      fail $ "Could not find any of the files: " <>
      show filepath1 <> ", " <>
      show filepath2 <> ", or " <>
      show filepath3
    liftIO $ putStrLn $ "loaded file " <> show path
    loadStreamEditText h
    liftIO $ hClose h
    cursorToEnd Before
    linum <- lineNumber
    liftIO $ putStrLn $
      "moved cursor to start of buffer, cursor = " <> show linum <>
      "\n"
    foldLinesFromCursor After showLinesNumbered (1::Int)
    liftIO $ putStrLn $
      "--------------------------------------------" <>
      "------------------------\nprint in reverse\n" <>
      "cursorToEnd After ..."
    cursorToEnd After
    linum <- lineNumber
    liftIO $ putStrLn $
      "moved cursor to end of buffer, cursor = " <> show linum
    foldLinesFromCursor Before showLinesNumbered (1::Int)
  case result of
    Right _i -> pure buf
    Left err -> fail $ "buffering cabal file failed: " <> show err

----------------------------------------------------------------------------------------------------

data SExpr
  = StringLit !StringData
  | CharLit !StringData
  | NumberLit !StringData
  | Keyword !StringData
  | SymLit !StringData
  | Quote !SExpr
  | Backquote !SExpr
  | Hashed !SExpr
  | Form !(Vector SExpr)
  deriving Eq

instance Show SExpr where
  show = \ case
    StringLit str -> convertString str
    CharLit   str -> '?' : convertString str
    NumberLit str -> convertString str
    Keyword   str -> convertString str
    SymLit    str -> convertString str
    Quote     str -> '\'' : show str
    Backquote str -> '`' : show str
    Hashed    str -> '#' : show str
    Form      str -> '(' : unwords (show <$> Vec.toList str) <> ")"

instance DisplayInfo SExpr where
  displayInfo = displayInfoShow

closeOf :: Char -> Char
closeOf = \ case
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  c   -> c

sexpr :: StringParser IO SExpr
sexpr =
  let makestr = toStringData . Just in
  choice
  [ ( oneOf "([{" >>= \ bracket -> pure ("}])"::String) >>
      Form . Vec.fromList <$> (many (spaces >> sexpr) <* char (closeOf bracket))
    ) <?> "form"
  , ( char '?' >>
      CharLit . makestr <$>
      ( ((\ a b -> a:b:"") <$> char '\\' <*> oneOf "abfnrtv") <|>
        ((:"") <$> anyChar)
      )
    ) <?> "character literal"
  , ( char '"' >>
      StringLit . makestr . ("\"" <>) . (<> "\"") . join <$>
      manyTill
      ( many (satisfy (\ c -> c /= '\\' && c /= '"')) >>= \ str ->
        ((char '\\' >> pure (str <> "\\")) <|> pure str)
      )
      (char '"')
    ) <?> "string literal"
  , ( Keyword . makestr <$>
      ((:) <$> char ':' <*> some (satisfy (not . isSpace)))
    ) <?> "keyword"
  , ( NumberLit . makestr <$>
      ( ((:"") <$> oneOf "+-" <|> pure "") >>= \ sign ->
        some (satisfy isDigit) >>= \ num ->
        ((:) <$> char '.' <*> some (satisfy isDigit) <|> pure "") >>= \ decpoint ->
        ( (:) <$> oneOf "eE" <*>
          ((<>) <$> ((:"") <$> oneOf "+-" <|> pure "") <*> some (satisfy isDigit)) <|>
          pure ""
        ) >>= \ pow ->
        pure (sign <> num <> decpoint <> pow)
      )
    ) <?> "number literal"
  , char '\'' >> (Quote <$> sexpr <?> "quoted s-expression")
  , char '`' >> (Backquote <$> sexpr <?> "backquoted s-expression")
  , char '#' >> (Hashed <$> sexpr <?> "hashed s-expression")
  , SymLit . makestr <$>
    some (satisfy (\ c -> not $ isSpace c || elem c ("([{'`\"#?:;}])"::String))) <?>
    "symbol literal"
  ] <?> "s-expression"

parseTest
  :: Show a
  => StringParser IO a
  -> StringParserState
  -> IO (StringParserResult IO a, StringParserState)
parseTest p st = do
  (result, st) <- runStringParser p st
  displayInfoPrint st
  print result
  putStrLn ""
  pure (result, st)

parseStep
  :: Show a
  => StringParserResult IO a
  -> StringParserState
  -> IO (StringParserResult IO a, StringParserState)
parseStep p st = case p of
  ParseWait p -> do
    (result, st) <- runStringParser p st
    displayInfoPrint st
    print result
    putStrLn ""
    pure (result, st)
  p -> print p >> pure (p, st)

newParse :: String -> StringParserState
newParse str =
  stringParserState
  { theParserIsEOF = False
  , theParserStream = toCharStream str charStreamEnd
  }

chvec :: String -> CharVector
chvec = convertString

testStringParser :: IO ()
testStringParser = do
  (a, st) <- parseTest sexpr $ newParse "(print \"Hello, world!\\n\"\n"
  (ParseOK a, _) <- parseStep a $ feedStringParser st True (chvec "  :i 1.5432e-3 :j 950)\n")
  let expect = Form $ Vec.fromList
        [ SymLit "print"
        , StringLit "\"Hello, world!\\n\""
        , Keyword ":i"
        , NumberLit "1.5432e-3"
        , Keyword ":j"
        , NumberLit "950"
        ]
  if a == expect then pure () else
    error $ "expected: " <> show expect <> "\nparsed: " <> show a

----------------------------------------------------------------------------------------------------

preambleAGPL :: Vector (Strict.Text, Strict.Text)
preambleAGPL =
  Vec.fromList $
  [ ("AA", "    The GNU Affero General Public  License is a free, copyleft license")
  , ("AB", "for software and other kinds of works, specifically designed to ensure")
  , ("AC", "cooperation with the community in the case of network server software.")
  , ("AD", "    The  licenses for  most  software and  other  practical works  are")
  , ("AE", "designed to take away your freedom  to share and change the works.  By")
  , ("AF", "contrast, our General  Public Licenses are intended  to guarantee your")
  , ("AG", "freedom to share and change all versions of a program--to make sure it")
  , ("AH", "remains free software for all its users."                              )
  , ("AI", "    When we speak  of free software, we are referring  to freedom, not")
  , ("AJ", "price.  Our General Public Licenses are designed to make sure that you")
  , ("AK", "have the freedom to distribute copies of free software (and charge for")
  , ("AL", "them if you wish),  that you receive source code or can  get it if you")
  , ("AM", "want it, that you  can change the software or use pieces  of it in new")
  , ("AN", "free programs, and that you know you can do these things."             )
  , ("AO", "    Developers  that  use our  General  Public  Licenses protect  your")
  , ("AP", "rights with two  steps: (1) assert copyright on the  software, and (2)")
  , ("AQ", "offer  you this  License which  gives  you legal  permission to  copy,")
  , ("AR", "distribute and/or modify the software."                                )
  , ("AS", "    A  secondary  benefit of  defending  all  users' freedom  is  that")
  , ("AT", "improvements  made  in alternate  versions  of  the program,  if  they")
  , ("AU", "receive  widespread  use, become  available  for  other developers  to")
  , ("AV", "incorporate.   Many  developers of  free  software  are heartened  and")
  , ("AW", "encouraged  by the  resulting cooperation.   However, in  the case  of")
  , ("AX", "software used on network servers, this  result may fail to come about.")
  , ("AY", "The GNU General  Public License permits making a  modified version and")
  , ("AZ", "letting the  public access it on  a server without ever  releasing its")
  , ("BA", "source code to the public."                                            )
  , ("BB", "    The GNU Affero General Public  License is designed specifically to")
  , ("BC", "ensure that, in such cases, the modified source code becomes available")
  , ("BD", "to the  community.  It requires  the operator  of a network  server to")
  , ("BE", "provide the source  code of the modified version running  there to the")
  , ("BF", "users of that server.  Therefore, public use of a modified version, on")
  , ("BG", "a publicly  accessible server, gives  the public access to  the source")
  , ("BH", "code of the modified version."                                         )
  , ("BI", "    An older  license, called  the Affero  General Public  License and")
  , ("BJ", "published by Affero,  was designed to accomplish  similar goals.  This")
  , ("BK", "is a  different license, not a  version of the Affero  GPL, but Affero")
  , ("BL", "has released a new version of the Affero GPL which permits relicensing")
  , ("BM", "under this license."                                                   )
  ]

-- | Selecting only lines that contain the words "software", "license", or "GNU".
preambleAGPLFound :: Vector (Strict.Text, Strict.Text)
preambleAGPLFound =
  Vec.fromList
  [ ("AA", "    The GNU Affero General Public  License is a free, copyleft license")
  , ("AB", "for software and other kinds of works, specifically designed to ensure")
  , ("AC", "cooperation with the community in the case of network server software.")
  , ("AD", "    The  licenses for  most  software and  other  practical works  are")
  , ("AF", "contrast, our General  Public Licenses are intended  to guarantee your")
  , ("AH", "remains free software for all its users."                              )
  , ("AI", "    When we speak  of free software, we are referring  to freedom, not")
  , ("AJ", "price.  Our General Public Licenses are designed to make sure that you")
  , ("AK", "have the freedom to distribute copies of free software (and charge for")
  , ("AM", "want it, that you  can change the software or use pieces  of it in new")
  , ("AO", "    Developers  that  use our  General  Public  Licenses protect  your")
  , ("AP", "rights with two  steps: (1) assert copyright on the  software, and (2)")
  , ("AQ", "offer  you this  License which  gives  you legal  permission to  copy,")
  , ("AR", "distribute and/or modify the software."                                )
  , ("AV", "incorporate.   Many  developers of  free  software  are heartened  and")
  , ("AX", "software used on network servers, this  result may fail to come about.")
  , ("AY", "The GNU General  Public License permits making a  modified version and")
  , ("BB", "    The GNU Affero General Public  License is designed specifically to")
  , ("BI", "    An older  license, called  the Affero  General Public  License and")
  , ("BK", "is a  different license, not a  version of the Affero  GPL, but Affero")
  , ("BM", "under this license."                                                   )
  ]

-- | Removing any lines of text that contain the words "software", "license", or "GNU".
preambleAGPLNotFound :: Vector (Strict.Text, Strict.Text)
preambleAGPLNotFound =
  Vec.fromList
  [ ("AE", "designed to take away your freedom  to share and change the works.  By")
  , ("AG", "freedom to share and change all versions of a program--to make sure it")
  , ("AL", "them if you wish),  that you receive source code or can  get it if you")
  , ("AN", "free programs, and that you know you can do these things."             )
  , ("AS", "    A  secondary  benefit of  defending  all  users' freedom  is  that")
  , ("AT", "improvements  made  in alternate  versions  of  the program,  if  they")
  , ("AU", "receive  widespread  use, become  available  for  other developers  to")
  , ("AW", "encouraged  by the  resulting cooperation.   However, in  the case  of")
  , ("AZ", "letting the  public access it on  a server without ever  releasing its")
  , ("BA", "source code to the public."                                            )
  , ("BC", "ensure that, in such cases, the modified source code becomes available")
  , ("BD", "to the  community.  It requires  the operator  of a network  server to")
  , ("BE", "provide the source  code of the modified version running  there to the")
  , ("BF", "users of that server.  Therefore, public use of a modified version, on")
  , ("BG", "a publicly  accessible server, gives  the public access to  the source")
  , ("BH", "code of the modified version."                                         )
  , ("BJ", "published by Affero,  was designed to accomplish  similar goals.  This")
  , ("BL", "has released a new version of the Affero GPL which permits relicensing")
  ]

-- | All-caps lines that contain the string "free"
preambleAGPLLoudFree :: Vector (Strict.Text, Strict.Text)
preambleAGPLLoudFree =
  Vec.fromList
  [ ("AE", "DESIGNED TO TAKE AWAY YOUR FREEDOM  TO SHARE AND CHANGE THE WORKS.  BY")
  , ("AG", "FREEDOM TO SHARE AND CHANGE ALL VERSIONS OF A PROGRAM--TO MAKE SURE IT")
  , ("AL", "them if you wish),  that you receive source code or can  get it if you")
  , ("AN", "FREE PROGRAMS, AND THAT YOU KNOW YOU CAN DO THESE THINGS."             )
  , ("AS", "    A  SECONDARY  BENEFIT OF  DEFENDING  ALL  USERS' FREEDOM  IS  THAT")
  , ("AT", "improvements  made  in alternate  versions  of  the program,  if  they")
  , ("AU", "receive  widespread  use, become  available  for  other developers  to")
  , ("AW", "encouraged  by the  resulting cooperation.   However, in  the case  of")
  , ("AZ", "letting the  public access it on  a server without ever  releasing its")
  , ("BA", "source code to the public."                                            )
  , ("BC", "ensure that, in such cases, the modified source code becomes available")
  , ("BD", "to the  community.  It requires  the operator  of a network  server to")
  , ("BE", "provide the source  code of the modified version running  there to the")
  , ("BF", "users of that server.  Therefore, public use of a modified version, on")
  , ("BG", "a publicly  accessible server, gives  the public access to  the source")
  , ("BH", "code of the modified version."                                         )
  , ("BJ", "published by Affero,  was designed to accomplish  similar goals.  This")
  , ("BL", "has released a new version of the Affero GPL which permits relicensing")
  ]

pairRowLabelValue :: Table.Row a -> (Strict.Text, a)
pairRowLabelValue row = (Table.theRowLabel row, Table.theRowValue row)

textContainsAnyOf :: Strict.Text -> Strict.Text -> Bool
textContainsAnyOf syms0 input0 =
  or $ flip elem syms <$> input
  where
  lwords =
    Strict.words .
    Strict.filter
    (\ c -> isLower c || isSpace c) .
    Strict.map toLower
  syms  = Vec.fromList $ lwords syms0
  input = lwords input0

setEqVectors :: Ord a => Vector a -> Vector a -> Bool
setEqVectors a b = set a == set b
  where
  set = Set.fromList . Vec.toList

showTestVec :: Vector (Strict.Text, Strict.Text) -> IO ()
showTestVec vec =
  forM_ (zip [0::Int ..] $ Vec.toList vec) $ \ (i, (lbl, txt)) ->
  putStrLn $ ralign6 i <> ": " <> Strict.unpack lbl <> ": " <> show txt

tableTests :: IO ()
tableTests = do
  table <- Table.new 4
  let testFilter = textContainsAnyOf "GNU software licenses license"
  ((), table) <-
    flip Table.exec table $
    mapM_ (uncurry Table.insert) $
    Vec.toList preambleAGPL
  (found, table) <-
    flip Table.exec table $
    fmap (fmap pairRowLabelValue) $
    Table.list $
    pure . testFilter . Table.theRowValue
  if setEqVectors found preambleAGPLFound
    then
      putStrLn "TEST PASSED: Table.insert, Table.list"
    else do
      putStrLn $ "'Table.list'\nexpected result (order not important):"
      showTestVec preambleAGPLFound
      putStrLn $ "computed result (order not important):"
      showTestVec found
      putStrLn "FAILED TEST"
  (found, table) <-
    flip Table.exec table $
    fmap pairRowLabelValue <$>
    Table.select1
    (textContainsAnyOf "goals" . Table.theRowValue)
  let expected = preambleAGPL Vec.! 35
  case found of
    Just pair | pair == expected ->
      putStrLn "TEST PASSED: Table.select1"
    found ->
      putStrLn $
      "expected result:\nJust " <> show expected <>
      "\nactual result: " <> show found <>
      "\nTEST FAILED: Table.select1"
  let expected =
        ( Table.UpdateResult
          { Table.manyKept    = 14
          , Table.manyRemoved = 21
          , Table.manyUpdated = 4
          }
        , preambleAGPLLoudFree
        )
  (result, _table) <-
    flip Table.exec table $
    (,) <$>
    Table.update
    (\ row -> pure $
      let txt = Table.theRowValue row in
      if testFilter txt
      then Table.ItemRemove
      else if textContainsAnyOf "free freedom" txt
      then Table.ItemUpdate $ Table.rowValue .~ Strict.map toUpper txt $ row
      else Table.ItemKeep
    ) <*>
    ( fmap pairRowLabelValue <$>
      Table.list (const $ pure True)
    )
  if fst result == fst expected && setEqVectors (snd result) (snd expected) then
      putStrLn "TEST PASSED: Table.update"
    else do
      putStrLn $ "expected value: " <> show (fst expected)
      showTestVec (snd expected)
      putStrLn $ "actual result: " <> show (fst result)
      showTestVec (snd result)
  pure ()

----------------------------------------------------------------------------------------------------

-- | Create a new 'Manager' state and use it just once to evaluate the given 'Manager' continuation.q
withNewManager :: Manager a -> IO a
withNewManager = (newManagerEnv >>=) . runManager

-- | Run all tests.
main :: IO ()
main = do
  --------------------------
  -- Basic tests, sanity checking.
  testEditor
  testGapBuffer
  --------------------------
  -- Tests that run in a manager environment
  env <- newManagerEnv
  flip runManager env $ do
    testTextEditor
    testByteStreamToLines
    bufferCabalFile
  --------------------------
  -- Parser tests
  testStringParser
  --------------------------
  -- Table tests
  tableTests
  --------------------------
  pure ()
