{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Control.Foldl as Fold
import qualified Data.Text as TS

main :: IO ()
main = sh $ do
    aSubmDir <- ls submDir
    aSubm <- find (suffix "sources.cm") aSubmDir
    let implDir = directory aSubm
    cd implDir
    cp runScript "./run.sml"
    exitCode <-
      proc "sml" ["run.sml",  testZero] (select [])
    --print $ (encodeString aSubmDir) ++ "\n\t" ++ show exitCode
    output 
      (decodeString "report.txt") 
      (return . stringToLine $ show exitCode ++ " -- " ++ encodeString (basename aSubmDir))
    return ()

baseDir :: String
baseDir = "/home/ulysses/c/cs6410-compilers-TA"

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/submissions/a1/assignment_1492"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tiger-testcases"

testZero :: Text
testZero = TS.pack $
  baseDir ++ "/mine/tiger-testcases/test.tig"

runScript :: FilePath
runScript = decodeString $
  baseDir ++ "/mine/tiger-my/chap2/run.sml"

pathToLine :: FilePath -> Line
pathToLine = fromJust . textToLine . fromRight "" . toText

stringToLine :: String -> Line
stringToLine = fromJust . textToLine . TS.pack

{-
newtype Comment = C [CommentContent]
data CommentContent 
  = CS String 
  | CC Comment

controlCodes = "ABCDEFJHIJKLMNOPQRSTUVXYZ[\\]^_?"

newtype StringLit = S [StringLitContent]
data StringLitContent
  = C Char
  | NL -- '\n'
  | TB -- '\t'
  | CN Int -- control code n is '\^c' where c = controlCodes !! n
  | AS Int -- 3-digit ascii code n: '\n1n2n3'
  | QT -- the double-quote '\"'
  | BS -- the backslash
  | IG String -- ignored sequence of non-printable chars:
              -- space, newline, tab, formfeed — between a pair of backslashes

data Lexem =
  | TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | THEN
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | DOT
  | RBRACE
  | LBRACE
  | RBRACK
  | LBRACK
  | RPAREN
  | LPAREN
  | SEMICOLON
  | COLON
  | COMMA
  | INT Int
  | ID String
--  | EOF -- I'm not sure we want explicit EOF
-- Here are things that are discarded or significantly transformed by lexers
  | COMMENT Comment
  | STRING StringLit
-}
