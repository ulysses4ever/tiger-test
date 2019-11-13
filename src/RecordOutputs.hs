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
import Data.String.Interpolate (i)
import Data.ByteString.Lazy.Search (replace)

{-
--  Recording outputs
-}

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
    let linesOut = dropWhileNotStart . BSC8.lines $ outRaw
        out      = BSC8.unlines . tail $ linesOut
        outFile  = outFileName odir ".out" test
    when (length linesOut == 0)
      $ fail "Program failed before reaching START marker"
    unless (BSC8.null out) $
      TBS.output outFile (pure out)
    unless (BSC8.null errRaw) $
      TBS.output (outFileName odir ".err" test) (pure errRaw)

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
dropWhileNotStart = dropWhile (not . (startMarker `BSC8.isPrefixOf`))

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: FilePath -> Text -> FilePath -> FilePath
outFileName odir ext inp = odir </> textToPath (inpFName `TS.append` ext)
  where
    inpFName = pathToText . filename $ inp
