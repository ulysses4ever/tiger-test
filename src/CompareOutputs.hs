{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE QuasiQuotes       #-}

module CompareOutputs where

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

{------------------------
--
--  Comparing outputs
--
-------------------------}

{-------------------------------------------------------------------------------
-
-                                Pure
-
-------------------------------------------------------------------------------}

type LineNum = Int
data AlignResult 
    = Match
    | NotMatch LineNum ByteString {- <- reference -} ByteString {- <- actual -}
    | Truncated
    deriving (Show)

align :: Int -> [ByteString] -> [ByteString] -> AlignResult
align _n [] _actual = Match
align _n _refecernce [] = Truncated
align n (r:rs) (a:as) =
  case matchLines n r a of
    Match -> align (n+1) rs as
    nm@NotMatch{} -> nm

matchLines :: Int -> ByteString -> ByteString -> AlignResult
matchLines n r a
    | r `eqLines` a = Match
    | otherwise     = NotMatch n r a

eqLines :: ByteString -> ByteString -> Bool
eqLines (BSC8.words -> rs) (BSC8.words -> as)
  = and $ (length rs == length as) : zipWith eqWords rs as

eqWords :: ByteString -> ByteString -> Bool
eqWords r a 
  =  invertTrueFalse a == r 
  || r == a

invertTrueFalse :: ByteString -> ByteString
invertTrueFalse w = BSL.toStrict res where
  dummy = "X*X*X"
  noTrue = replace "true" dummy (BSL.fromStrict w)
  noFalse = replace @BS.ByteString  "false" "true" noTrue
  res = replace @BS.ByteString dummy "false" noFalse

firstToken :: ByteString -> ByteString
firstToken = id -- BSC8.toUpper -- . TS.takeWhile isAlpha


{-------------------------------------------------------------------------------
-
-                                Shell I/O
-
-------------------------------------------------------------------------------}

prepareTesting :: Shell ()
prepareTesting = do
    touch reportFilename
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
    -- liftIO . print $ "Comparing: " `TS.append` pathToText outFile
    actOut <- TBS.input outFile
    refOut  <- TBS.input (referenceDir </> filename outFile)
    storeResult (filename outFile) $ align 1 (BSC8.lines refOut) (BSC8.lines actOut)
    -- TODO: check for .err files in the should_work path

-- Compare ouputs from the current submission with the reference ones
-- and produce "report.txt"
checkOutputs :: FilePath -> IO ()
checkOutputs odir = sh $ do
    enumerateSubmissions
    prepareTesting
    compareWithReference odir
