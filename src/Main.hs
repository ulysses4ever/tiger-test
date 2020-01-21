module Main where

import StringUtils
import CompareOutputs
import Constants
import Core
import RecordOutputs

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

main :: IO ()
main = do
  collectOutputs testsShouldWorkDir outDir
  print "DONE: recording output for should-work tests."
{-
  collectOutputs testsShouldFailDir outFailDir
  print "DONE: recording output for should-fail tests."
--}
{-
  checkSetOutputs outDir
  checkSetOutputs outFailDir
--}
  print "DONE: testing."
