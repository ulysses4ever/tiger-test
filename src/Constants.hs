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
phase = "parse"

outDir :: IsString s => s
outDir = "_out"

outFailDir :: IsString s => s
outFailDir = "_out_fail"

startMarker :: ByteString
startMarker = "START_" `BSC8.append` BSC8.map toUpper phase

reportFilename :: IsString s => s
reportFilename = "report.txt"

baseDir :: String
baseDir = "/home/ulysses/Documents/classes/cs6410-compilers-TA"

referenceDir :: FilePath
referenceDir = decodeString $
  baseDir ++ "/submissions/a2/517762_Chung_Brandon/Archive2/" ++ outDir

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
  baseDir ++ "/mine/tiger-my/runners/" ++ phase ++ "/run.sml"
