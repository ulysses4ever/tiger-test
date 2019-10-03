{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle hiding (textToLine)
import Prelude hiding (FilePath)
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import qualified Control.Foldl as Fold
import qualified Data.Text as TS

main :: IO ()
main = 
  (sh $ do
    prepareSubmissionForTest

    -- Run a submission over what should lex and record results in "_out" subdir
    test <- tigerFiles testsShouldWorkDir
    _ <- recordOutput test
    return ()) *> print "Done testing."

-- enumerating all submissions and for each one:
-- cd into it, cp the driver (run.sml) into it, create `_out` subdir
prepareSubmissionForTest :: Shell ()
prepareSubmissionForTest = do
    aSubmDir <- ls submDir
    aSubm <- find (suffix "sources.cm") aSubmDir
    let implDir = directory aSubm
    cd implDir
    cp runScript "./run.sml"
    mktree outDir
    liftIO . print $ "Processing " ++ (pathToString aSubmDir)

-- record output of a submission stored at FilePath, when run of the whole suite
recordOutput :: FilePath -> Shell ()
recordOutput test = do
    liftIO . print $ "Testing: " `TS.append` pathToText test
    (_exitCode, outRaw, errRaw) <-
      procStrictWithErr "sml" ["run.sml",  pathToText test] (select [])
    let linesOut = select . map textToLine . dropWhileNotStart . TS.lines $ outRaw
        linesErr = select . map textToLine . TS.lines $ errRaw
    output (outFileName ".out" test) linesOut
    when (not . TS.null $ errRaw) $ output (outFileName ".err" test) linesErr

-- cuts head of stream of lines from stderr/stdout of a submission,
-- while not see the start_lexer marker in the stdout (the second stream)
dropWhileNotStart :: [Text] -> [Text]
dropWhileNotStart = tail . dropWhile (not . ("LEXER_START" `TS.isPrefixOf`))

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: Text -> FilePath -> FilePath
outFileName ext inp = outDir </> textToPath (inpFName `TS.append` ext)
  where
    inpFName = pathToText . filename $ inp

{-
--       Hard-coded paths and constants
-}

baseDir :: String
baseDir = "/home/ulysses/c/cs6410-compilers-TA"

outDir :: IsString s => s
outDir = "_out"

phase :: IsString s => s
phase = "lex"

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/submissions/a1/assignment_1492"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tiger-testcases"

testsShouldWorkDir :: FilePath
testsShouldWorkDir = testDir </> phase </> "should_work"

testsShouldFailDir :: FilePath
testsShouldFailDir = testDir </> phase </> "should_fail"

runScript :: FilePath
runScript = decodeString $
  baseDir ++ "/mine/tiger-my/chap2/run.sml"

{-
--       I hate string-like type conversions
-}

pathToLine :: FilePath -> Line
pathToLine = textToLine . fromRight "" . toText

stringToLine :: String -> Line
stringToLine = textToLine . TS.pack

textToLine :: Text -> Line
textToLine = unsafeTextToLine

stringToPath :: String -> FilePath
stringToPath = decodeString

pathToString :: FilePath -> String
pathToString = encodeString

textToPath :: Text -> FilePath
textToPath = fromText

pathToText :: FilePath -> Text
pathToText = fromRight "ERRORING on FilePath->Text conv" . toText

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
