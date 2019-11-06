{-# LANGUAGE OverloadedStrings #-}

module StringUtils where

{-
--       I hate string-like type conversions
-}
import Prelude hiding (FilePath)
import Data.Text (Text)
import qualified Data.Text as TS
import Turtle (FilePath, Line, toText, fromText, unsafeTextToLine, encodeString, decodeString)

import Data.Either (fromRight)

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
