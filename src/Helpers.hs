module Helpers where

import StringUtils

import Turtle hiding (textToLine)
import Prelude hiding (FilePath)

import Data.Set (Set, fromDistinctAscList, elems)

shellToList
  :: MonadIO io
  => Shell a -> io [a]
shellToList sh = fold sh (Fold (flip (:)) [] id)

lsSetNames
  :: MonadIO io
  => FilePath -> io (Set Text)
lsSetNames p =
  fromDistinctAscList <$>
  (map pathToText <$>
    sort (
      fmap filename $ ls p))

setToLines :: Set Text -> Shell Line
setToLines = select . map unsafeTextToLine . elems
