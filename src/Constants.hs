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

phase :: IsString s => s
phase = "w3"

outDir :: IsString s => s
outDir = "_out"

outFailDir :: IsString s => s
outFailDir = "_out_fail"

runScriptName :: IsString s => s
runScriptName = "test.cpp"

startMarker :: ByteString
startMarker = "START_" `BSC8.append` BSC8.map toUpper phase

reportFilename :: FilePath -> FilePath
reportFilename odir = odir </> "report.txt"

baseDir :: String
baseDir = "/home/artem/Classes/cs4500"

referenceDir :: FilePath -> FilePath
referenceDir odir = (decodeString $
  baseDir ++ "/subm/w3/???") </> odir

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/subm/w3/assignment_1826"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tests/" ++ phase

testsShouldWorkDir :: FilePath
testsShouldWorkDir = testDir </> phase </> "should_work"

testsShouldFailDir :: FilePath
testsShouldFailDir = testDir </> phase </> "should_fail"

runScript :: FilePath
runScript = decodeString $
  baseDir ++ "/mine/runners/" ++ phase ++ "/test.cpp"

dockerfile :: FilePath
dockerfile = decodeString $
  baseDir ++ "/mine/runners/" ++ phase ++ "/Dockerfile"

--------------------------------------------------------
--
--  Commands
--
--------------------------------------------------------

build :: IsString s => s
build = "make"

exe :: IsString s => s
exe = "./a.out"

--------------------------------------------------------
--
--  Tests
--
--------------------------------------------------------


type Test = (Int, (String, String)) -- Id & Input & Output

tests :: [Test]
tests = zip [1..]
  [ ("doc0.txt", "alanon 2")
  , ("doc2.txt", "going 70")
  ]
