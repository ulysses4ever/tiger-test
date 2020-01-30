{-#LANGUAGE OverloadedStrings #-}

module RecordOutputs where

import StringUtils
import Constants
import Core

import Turtle hiding (textToLine)
import qualified Turtle.Bytes as TBS
import Prelude hiding (FilePath)
import Data.Char (isAlpha, isDigit, toUpper)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)

import qualified Control.Foldl as Fold
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TSE
import Data.String.Interpolate (i)
import Data.ByteString.Lazy.Search (replace)

debugPrint :: Bool
debugPrint = True

{-
--  Recording outputs
-}

-- cp essential stuff, create `_out` subdir, pre-build actions
prepareCollectingOutput :: FilePath -> Shell ()
prepareCollectingOutput odir = do
  echoDebug "Preparing..."

  -- copy over testing stuff
    -- cp runScript runScriptName
  cp timeoutScript timeoutScriptName
  echoDebug "Copying over the tests"
  sh $
    ls testDir >>=
    \f -> (echo (pathToLine f) >> cp f (filename f))

  -- make the output dir
  mktree odir

  -- build docker image
  -- img <- dockerImageName
  --echoDebug (textToLine $ "Building the Docker image " `TS.append` img)
  exitCode <- proc
    "docker"
    ["build", "-t", dockerImageName `TS.append` ":0.1", "."]
    emptyInput
  unless
    (case exitCode of ExitSuccess -> True ; _ -> False)
    (echo "FAILED to build Docker image" >> fail "kaput")
  
    
run ::
  MonadIO m =>
  String -> m (ExitCode, ByteString, ByteString)
run inp = do
  wd <- pwd
  TBS.procStrictWithErr
    "docker"
    [
      "run",
      "-t",
      "-v",
      pathToText wd `TS.append` ":/test",
      dockerImageName `TS.append` ":0.1",
      "bash",
      "-c",
      TS.unwords   [
        "cd /test ; make build ; ",
        " if [ $? -ne '0' ]  ; then make docker ; fi ",
        --" ; if [ -f 'sorer' ]  ; then dos2unix sorer ; fi ",
        " && echo " ,
        TSE.decodeUtf8 startMarker,
        TS.unwords [" && ./timeout.sh -t ", TS.pack $ show timeout,  "  ./sorer -f "],
        TS.pack inp
      ]
    ]
    emptyInput

-- record output of a submission stored at FilePath, when run of the whole suite
recordOutput :: FilePath -> Test -> Shell ()
recordOutput odir (id, (i, o)) = do
    echo $ stringToLine $ "Test #" ++ (show id)

    (_exitCode, outRaw, errRaw) <- liftIO $ run i
    liftIO $ BSC8.putStrLn errRaw
    --liftIO $ print $ "Exit code: " ++ show exitCode
    --liftIO $ print $ "Output: " ++ show outRaw
    let linesOut = dropWhileNotStart . BSC8.lines $ outRaw
        firstLineOut = BSC8.filter (\c -> c /= '"') $ head (tail linesOut)
        linesOutCnt = length linesOut
        out      = BSC8.unlines . tail $ linesOut
        outFile  = outFileName odir ".stdout" id 
        errFile  = outFileName odir ".stderr" id 
        resFile  = outFileName odir ".res" id
        res | linesOutCnt == 0  = "Failed to compile\n"
            | linesOutCnt == 1  = "Empty output\n"
            | BSC8.pack o `BSC8.isPrefixOf` firstLineOut
              = "OK\n"
            | otherwise = "FALSE\n"
    TBS.output outFile (pure outRaw)
    TBS.output errFile (pure errRaw)
    liftIO $
      writeTextFile resFile
        (TS.unwords $ map TS.pack [pad 2 $ show id, " -- ", res])
    

collectOutputs :: FilePath -> FilePath -> IO ()
collectOutputs _ odir = sh $ do
    enumerateSubmissions
    prepareCollectingOutput odir

    mkExists <- testfile "Makefile"
    when (not mkExists) --(pure ())
      (echo "No Makefile" >>
        (liftIO $ writeTextFile
          (outFileName odir ".res" 0) "No Makefile"))

    echoDebug "About to run actual tests on the current submission"
    -- Run a submission over the tests and record results in "_out" subdir
    mapM_ (recordOutput odir) tests

{-
--  Helpers
-}

emptyInput :: Shell a
emptyInput = select []

-- cuts head of stream of lines from stderr/stdout of a submission,
-- while not see the start_lexer marker in the stdout (the second stream)
dropWhileNotStart :: [ByteString] -> [ByteString]
dropWhileNotStart = dropWhile (not . (startMarker `BSC8.isPrefixOf`))

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: FilePath -> Text -> Int -> FilePath
outFileName odir ext ind = odir </> textToPath (inpFName `TS.append` ext)
  where
    inpFName = TS.pack $ pad 2 ind

{-
dockerImageName :: MonadIO m => m Text
dockerImageName = do
  wdFull <- pwd
  let wd = filename (dirname wdFull)
      imgName = TS.toLower $ pathToText wd
  return imgName
-}
dockerImageName :: Text
dockerImageName = "assignment1"

echoDebug :: MonadIO m => Line -> m ()
echoDebug = if debugPrint then echo else const (pure ())

pad n = (\x -> replicate (n - length x) '0' ++ x)

