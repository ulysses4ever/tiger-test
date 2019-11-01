{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Turtle hiding (textToLine)
import qualified Turtle.Bytes as TBS
import Prelude hiding (FilePath)
import Data.Char (isAlpha, isDigit)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

import qualified Control.Foldl as Fold
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as TS
import Data.String.Interpolate (i)

main :: IO ()
main = do
  collectOutputs testsShouldWorkDir outDir
  print "DONE: recording output for should-work tests."
{-
  collectOutputs testsShouldFailDir outFailDir
  print "DONE: recording output for should-fail tests."
-}
{-
  checkOutputs outDir
-}
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

compareWithReference :: FilePath -> Shell ()
compareWithReference odir = do
    outFile <- find (suffix ".out") odir
    actOut <- liftIO $ readTextFile outFile
    refOut  <- liftIO $ readTextFile (referenceDir </> filename outFile)
    storeResult (filename outFile) $ align 1 (TS.lines refOut) (TS.lines actOut)
    -- TODO: check for .err files in the should_work path

-- Compare ouputs from the current submission with the reference ones
-- and produce "report.txt"
checkOutputs :: FilePath -> IO ()
checkOutputs odir = sh $ do
    enumerateSubmissions
    prepareTesting
    compareWithReference odir

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
prepareCollectingOutput :: FilePath -> Shell ()
prepareCollectingOutput odir = do
    cp runScript "./run.sml"
    mktree odir

-- record output of a submission stored at FilePath, when run of the whole suite
recordOutput :: FilePath -> FilePath -> Shell ()
recordOutput test odir = do
    liftIO . print $ "Testing: " `TS.append` pathToText test
    (_exitCode, outRaw, errRaw) <-
      TBS.procStrictWithErr "sml" ["run.sml",  pathToText test] (select [])
    let linesOut = pure . BSC8.unlines . dropWhileNotStart . BSC8.lines $ outRaw
        linesErr = pure . BSC8.unlines. BSC8.lines $ errRaw
    let outFile = (outFileName odir ".out" test)
    touch outFile
    TBS.output outFile linesOut
    when (not . BSC8.null $ errRaw) $ TBS.output (outFileName odir ".err" test) linesErr

collectOutputs :: FilePath -> FilePath -> IO ()
collectOutputs tests odir = sh $ do
    enumerateSubmissions
    prepareCollectingOutput odir

    -- Run a submission over the tests and record results in "_out" subdir
    test <- tigerFiles tests
    recordOutput test odir

{-
--  Helpers
-}

-- cuts head of stream of lines from stderr/stdout of a submission,
-- while not see the start_lexer marker in the stdout (the second stream)
dropWhileNotStart :: [ByteString] -> [ByteString]
dropWhileNotStart = tail . dropWhile (not . (startMarker `BSC8.isPrefixOf`))

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: FilePath -> Text -> FilePath -> FilePath
outFileName odir ext inp = odir </> textToPath (inpFName `TS.append` ext)
  where
    inpFName = pathToText . filename $ inp

{-
--       Hard-coded paths and constants
-}

outDir :: IsString s => s
outDir = "_out"

outFailDir :: IsString s => s
outFailDir = "_out_fail"

phase :: IsString s => s
phase = "parse"

startMarker :: IsString s => s
startMarker = "PARSER_START"

reportFilename :: IsString s => s
reportFilename = "report.txt"

baseDir :: String
baseDir = "/home/ulysses/Documents/classes/cs6410-compilers-TA"

referenceDir :: FilePath
referenceDir = decodeString $
  baseDir ++ "/submissions/a2/_MINE/" ++ outDir

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/submissions/a2/assignment_1559"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tiger-testcases"

testsShouldWorkDir :: FilePath
testsShouldWorkDir = testDir </> phase </> "should_work"

testsShouldFailDir :: FilePath
testsShouldFailDir = testDir </> phase </> "should_fail"

runScript :: FilePath
runScript = decodeString $
  baseDir ++ "/mine/tiger-my/chap3/run.sml"

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
