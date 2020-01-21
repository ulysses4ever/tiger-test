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


{-
--  Recording outputs
-}

-- cd into it, cp the driver (run.sml) into it, create `_out` subdir
prepareCollectingOutput :: FilePath -> Shell ()
prepareCollectingOutput odir = do
    -- cp runScript runScriptName
    mktree odir

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
      phase `TS.append` ":0.1",
      "bash",
      "-c",
      "cd /test ; make ; "
        `TS.append` "if [ $? -ne '0' ]  ; then make docker ; fi"
        `TS.append` " && echo "
        `TS.append` (TSE.decodeUtf8 startMarker)
        `TS.append` " && ./a.out -f "
        `TS.append` (TS.pack inp)
    ]
    (select [])

-- record output of a submission stored at FilePath, when run of the whole suite
recordOutput :: FilePath -> Test -> Shell ()
recordOutput odir (id, (i, o)) = do
    liftIO . print $ "Next test: " ++ i

    let iPath = stringToPath i
    cp (testDir </> iPath)  iPath
    (_exitCode, outRaw, errRaw) <- run i
    liftIO $ BSC8.putStrLn errRaw
    -- liftIO $ print $ "Exit code: " ++ show exitCode
    let linesOut = dropWhileNotStart . BSC8.lines $ outRaw
        linesOutCnt = length linesOut
        out      = BSC8.unlines . tail $ linesOut
        outFile  = outFileName odir ".stdout" id 
        errFile  = outFileName odir ".stderr" id 
        resFile  = outFileName odir ".res" id
        res | linesOutCnt == 0  = "Failed to compile"
            | linesOutCnt == 1  = "Empty output"
            | BSC8.pack o `BSC8.isPrefixOf` head (tail linesOut)
              = "OK"
            | otherwise = "FALSE"
    TBS.output outFile (pure outRaw)
    TBS.output errFile (pure errRaw)
    liftIO $ writeTextFile resFile res
    

collectOutputs :: FilePath -> FilePath -> IO ()
collectOutputs _ odir = sh $ do
    enumerateSubmissions
    prepareCollectingOutput odir

    mkExists <- testfile "Makefile"
    when (not mkExists) --(pure ())
      (liftIO $ writeTextFile
        (outFileName odir ".res" 0) "No Makefile")

    -- Run a submission over the tests and record results in "_out" subdir
    mapM_ (recordOutput odir) tests

{-
--  Helpers
-}

-- cuts head of stream of lines from stderr/stdout of a submission,
-- while not see the start_lexer marker in the stdout (the second stream)
dropWhileNotStart :: [ByteString] -> [ByteString]
dropWhileNotStart = dropWhile (not . (startMarker `BSC8.isPrefixOf`))

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: FilePath -> Text -> Int -> FilePath
outFileName odir ext id = odir </> textToPath (inpFName `TS.append` ext)
  where
    inpFName = TS.pack $ show id
