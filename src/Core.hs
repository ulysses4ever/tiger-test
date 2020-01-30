{-#LANGUAGE OverloadedStrings #-}
module Core where

import Constants
import StringUtils

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

-- enumerating all submissions and for each one:
enumerateSubmissions :: Shell ()
enumerateSubmissions = do
    cd submDir
    submList <- sort $ find (suffix "Makefile") submDir
    aSubm <- select submList
    let aSubmDir = directory aSubm
    cd aSubmDir
    liftIO . print $ "Processing " ++ pathToString aSubmDir

