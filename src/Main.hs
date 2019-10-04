{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Turtle hiding (textToLine)
import Prelude hiding (FilePath)
import Data.Char (isAlpha, isDigit)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import qualified Control.Foldl as Fold
import qualified Data.Text as TS
import Data.String.Interpolate (i)

main :: IO ()
main = do
  collectOutputs
  print "DONE: recording output."
  checkOutputs
  print "DONE: testing."

{-
--  Comparing results
-}

type LineNum = Int
data AlignResult 
    = Match
    | NotMatch LineNum Text {- <- reference -} Text {- <- actual -}
    | Truncated

align :: Int -> [Text] -> [Text] -> AlignResult
align _n [] _actual = Match
align _n _refecernce [] = Truncated
align n (r:rs) (a:as) =
  case matchLines n r a of
    Match -> align (n+1) rs as
    nm@(NotMatch _ _ _) -> nm

matchLines :: Int -> Text -> Text -> AlignResult
matchLines n (firstToken -> r) (firstToken -> a)
    | r == a    = Match
    | otherwise = NotMatch n r a

firstToken :: Text -> Text
firstToken = TS.toUpper . TS.takeWhile isAlpha

prepareTesting :: Shell ()
prepareTesting = do
    rm reportFilename
    storeSubmId

storeSubmId :: Shell ()
storeSubmId = append reportFilename sudmId
  where 
    sudmId :: Shell Line
    sudmId = textToLine
           . TS.dropWhile isDigit
           . TS.takeWhile (/= '/')
           . TS.drop (TS.length (pathToText submDir) + 1)
           . pathToText 
           <$> pwd

storeResult :: FilePath -> AlignResult -> Shell ()
storeResult _ Match = return ()
storeResult test Truncated = append reportFilename report 
  where
    report = return . stringToLine $
                pathToString test ++ ": truncated"
storeResult test (NotMatch line ref act) = append reportFilename report 
  where
    report = return . stringToLine $
      [i|#{pathToString test}: line #{show line}: expected "#{ref}", actual "#{act}"|]

compareWithReference :: Shell ()
compareWithReference = do
    outFile <- find (suffix ".out") outDir
    actOut <- liftIO $ readTextFile outFile
    refOut  <- liftIO $ readTextFile (referenceDir </> filename outFile)
    storeResult (filename outFile) $ align 1 (TS.lines refOut) (TS.lines actOut)

-- TODO: check no .err files in the should_work path
checkNoDotErrFiles = return () -- undefined

checkOutputs :: IO ()
checkOutputs = sh $ do
    enumerateSubmissions
    prepareTesting
    compareWithReference
    checkNoDotErrFiles

{-
--  Recording outputs
-}

-- enumerating all submissions and for each one:
enumerateSubmissions :: Shell ()
enumerateSubmissions = do
    aSubmDir <- ls submDir
    aSubm <- find (suffix "sources.cm") aSubmDir
    let implDir = directory aSubm
    cd implDir
    liftIO . print $ "Processing " ++ (pathToString aSubmDir)

-- cd into it, cp the driver (run.sml) into it, create `_out` subdir
prepareCollectingOutput :: Shell ()
prepareCollectingOutput = do
    cp runScript "./run.sml"
    mktree outDir

-- record output of a submission stored at FilePath, when run of the whole suite
recordOutput :: FilePath -> Shell ()
recordOutput test = do
    liftIO . print $ "Testing: " `TS.append` pathToText test
    (_exitCode, outRaw, errRaw) <-
      procStrictWithErr "sml" ["run.sml",  pathToText test] (select [])
    let linesOut = select . map textToLine . dropWhileNotStart . TS.lines $ outRaw
        linesErr = select . map textToLine . TS.lines $ errRaw
    let outFile = (outFileName ".out" test)
    touch outFile
    output outFile linesOut
    when (not . TS.null $ errRaw) $ output (outFileName ".err" test) linesErr

collectOutputs :: IO ()
collectOutputs = sh $ do
    enumerateSubmissions
    prepareCollectingOutput

    -- Run a submission over what should lex and record results in "_out" subdir
    test <- tigerFiles testsShouldWorkDir
    _ <- recordOutput test
    return ()

{-
--  Helpers
-}

-- cuts head of stream of lines from stderr/stdout of a submission,
-- while not see the start_lexer marker in the stdout (the second stream)
dropWhileNotStart :: [Text] -> [Text]
dropWhileNotStart = tail . dropWhile (not . ("LEXER_START" `TS.isPrefixOf`))

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: Text -> FilePath -> FilePath
outFileName ext inp = outDir </> textToPath (inpFName `TS.append` ext)
  where
    inpFName = pathToText . filename $ inp

{-
--       Hard-coded paths and constants
-}

outDir :: IsString s => s
outDir = "_out"

phase :: IsString s => s
phase = "lex"

reportFilename :: IsString s => s
reportFilename = "report.txt"

baseDir :: String
baseDir = "/home/ulysses/Documents/classes/cs6410-compilers-TA"

referenceDir :: FilePath
referenceDir = decodeString $
  baseDir ++ "/submissions/a1/_MINE/_out"

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/submissions/a1/assignment_1492"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tiger-testcases"

testsShouldWorkDir :: FilePath
testsShouldWorkDir = testDir </> phase </> "should_work"

testsShouldFailDir :: FilePath
testsShouldFailDir = testDir </> phase </> "should_fail"

runScript :: FilePath
runScript = decodeString $
  baseDir ++ "/mine/tiger-my/chap2/run.sml"

{-
--       I hate string-like type conversions
-}

pathToLine :: FilePath -> Line
pathToLine = textToLine . fromRight "" . toText

stringToLine :: String -> Line
stringToLine = textToLine . TS.pack

textToLine :: Text -> Line
textToLine = unsafeTextToLine

stringToPath :: String -> FilePath
stringToPath = decodeString

textToPath :: Text -> FilePath
textToPath = fromText

pathToText :: FilePath -> Text
pathToText = fromRight "ERRORING on FilePath->Text conv" . toText

stringToText :: String -> Text
stringToText = TS.pack

pathToString :: FilePath -> String
pathToString = encodeString

{-
newtype Comment = C [CommentContent]
data CommentContent 
  = CS String 
  | CC Comment

controlCodes = "ABCDEFJHIJKLMNOPQRSTUVXYZ[\\]^_?"

newtype StringLit = S [StringLitContent]
data StringLitContent
  = C Char
  | NL -- '\n'
  | TB -- '\t'
  | CN Int -- control code n is '\^c' where c = controlCodes !! n
  | AS Int -- 3-digit ascii code n: '\n1n2n3'
  | QT -- the double-quote '\"'
  | BS -- the backslash
  | IG String -- ignored sequence of non-printable chars:
              -- space, newline, tab, formfeed — between a pair of backslashes

data Lexem =
  | TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | THEN
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | DOT
  | RBRACE
  | LBRACE
  | RBRACK
  | LBRACK
  | RPAREN
  | LPAREN
  | SEMICOLON
  | COLON
  | COMMA
  | INT Int
  | ID String
--  | EOF -- I'm not sure we want explicit EOF
-- Here are things that are discarded or significantly transformed by lexers
  | COMMENT Comment
  | STRING StringLit
-}
