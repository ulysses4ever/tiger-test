{-# LANGUAGE OverloadedStrings #-}

module Constants where

{-----------------------------------------
--
--       Hard-coded paths and constants
--
------------------------------------------}

import Data.String (IsString)
import Data.Char   (toUpper)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as TS
import Prelude hiding (FilePath)
import Data.Text (Text)

import Turtle (FilePath, Line, toText, fromText, unsafeTextToLine, encodeString, decodeString, (</>))


{-----------------------------------------
--
--       Main ones (change frequently)
--
------------------------------------------}

phase :: IsString s => s
phase = "a1"

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/subm/" ++ phase ++ "/assignment_1834_res"

runScriptName :: IsString s => s
runScriptName = "test.cpp"

baseDir :: IsString s => s
baseDir = "/home/artem/Classes/cs4500"

{-----------------------------------------
--
--       Derived/fixed ones (hopefuly, change seldom)
--
------------------------------------------}

timeout :: Int
timeout = 60 -- in seconds

-- Those depending on my directory tree

referenceDir :: FilePath -> FilePath
referenceDir odir = (decodeString $
  baseDir ++ "/subm/ ++ phase ++ /ref") </> odir

thisDir :: FilePath
thisDir  = baseDir </> "tiger-test"

assetsDir :: FilePath
assetsDir = thisDir </> "assets"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tests/" ++ phase 

runScript :: FilePath
runScript =
  baseDir </> "mine/runners" </> phase </> runScriptName

timeoutScript :: FilePath
timeoutScript = 
  assetsDir </> timeoutScriptName

dockerfile :: FilePath
dockerfile = decodeString $
  baseDir ++ "/mine/runners/" ++ phase ++ "/Dockerfile"

-- Uninteresting

timeoutScriptName :: IsString s => s
timeoutScriptName = "timeout.sh"

outDir :: IsString s => s
outDir = "_out"

outFailDir :: IsString s => s
outFailDir = "_out_fail"


startMarker :: ByteString
startMarker = "START_" `BSC8.append` BSC8.map toUpper phase

reportFilename :: FilePath -> FilePath
reportFilename odir = odir </> "report.txt"

testsShouldWorkDir :: FilePath
testsShouldWorkDir = testDir </> phase </> "should_work"

testsShouldFailDir :: FilePath
testsShouldFailDir = testDir </> phase </> "should_fail"

--------------------------------------------------------
--
--  Commands
--
--------------------------------------------------------

build :: IsString s => s
build = "make build"

exe :: IsString s => s
exe = "./sorer"

--------------------------------------------------------
--
--  Tests
--
--------------------------------------------------------


type Test = (Int, (String, String)) -- Id & Input & Output

tests :: [Test]
tests = zip [1..]
  [ ("0.sor -from 0 -len 4 -print_col_type 0  ",
     "BOOL")

  , ("0.sor -from 0 -len 4 -is_missing_idx 0 0",
     "1")
  , ("1.sor -from 0 -len 76 -print_col_type 0  ",
     "STRING")
  , ("1.sor -from 0 -len 76 -is_missing_idx 0 1",
     "0")
  , ("1.sor -from 0 -len 76 -print_col_idx 0 3",
     "+1")
  , ("2.sor -from 0 -len 50 -print_col_type 0",
     "BOOL")
  , ("2.sor -from 0 -len 50 -print_col_type 1",
     "INTEGER")
  , ("2.sor -from 0 -len 50 -print_col_type 2",
     "FLOAT")
  , ("2.sor -from 0 -len 50 -print_col_type 3",
     "STRING")
  , ("2.sor -from 0 -len 50 -is_missing_idx 1 0",
     "1")
  , ("2.sor -from 0 -len 50 -is_missing_idx 1 1",
     "0")
  , ("2.sor -from 0 -len 50 -print_col_idx 3 0",
     "hi")
  , ("2.sor -from 0 -len 50 -print_col_idx 3 1",
     "ho ho ho")
  , ("2.sor -from 0 -len 50 -print_col_idx 2 0",
     "1.2")
  , ("2.sor -from 0 -len 50 -print_col_idx 2 1",
     "-.2")
  , ("3.sor -from 0 -len 30352432 -print_col_type 4",
     "BOOL")
  , ("3.sor -from 0 -len 30352432 -print_col_idx 2 10",
     "+1.2")
  , ("3.sor -from 0 -len 30352432 -print_col_idx 2 384200",
     "+1.2")
  , ("4.sor -from 0 -len 116 -print_col_idx 0 1",
     "+2147483647")
  , ("4.sor -from 0 -len 116 -print_col_idx 0 2",
     "-2147483648")
  , ("4.sor -from 0 -len 116 -print_col_idx 1 1",
     "-0.000000002")
  , ("4.sor -from 0 -len 116 -print_col_idx 1 2",
     "10000000000")
  , ("1.sor -from 1 -len 74 -print_col_type 0",
     "STRING")
  , ("1.sor -from 1 -len 74 -is_missing_idx 0 0",
     "0")
  , ("1.sor -from 1 -len 74 -print_col_idx 0 6",
     "+2.2")

  ]
