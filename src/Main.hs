{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
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
    test <- tigerFiles testsShouldLexDir
    _ <- recordOutput test
    return ()) *> print "Done testing."

prepareSubmissionForTest :: Shell ()
prepareSubmissionForTest = do
    aSubmDir <- ls submDir
    aSubm <- find (suffix "sources.cm") aSubmDir
    let implDir = directory aSubm
    cd implDir
    cp runScript "./run.sml"
    mktree "_out"
    liftIO . print $ "Processing " ++ (pathToString aSubmDir)

-- record output of a submission stored at FilePath, when run of the whole suite
recordOutput :: FilePath -> Shell ()
recordOutput test = do
    liftIO . print $ "Testing: " `TS.append` pathToText test
    let linesOutRaw = inprocWithErr "sml" ["run.sml",  pathToText test] (select [])
    (linesErrRev, linesOutRev) <- fold linesOutRaw dropWhileNotStart
    let linesOut = select $ reverse linesOutRev
        linesErr = select $ reverse linesErrRev
    when (not . null $ linesOutRev) $ output (outFileName ".out" test) linesOut
    when (not . null $ linesErrRev) $ output (outFileName ".err" test) linesErr

-- cuts head of stream of lines from stderr/stdout of a submission,
-- while not see the start_lexer marker in the stdout (the second stream)
dropWhileNotStart :: Fold (Either Line Line) ([Line], [Line])
dropWhileNotStart = Fold
    f                     -- step
    (([], []), False)     -- init
    fst                   -- extract
  where
    f ((errs, outs), {- started= -}True) eitherErrOut =
      (updateErrsOuts errs outs eitherErrOut, True)
    f (lines, False) eitherErrOut =
          (lines, checkStarted eitherErrOut)
    checkStarted (Left l) = False
    checkStarted (Right l) =
        ("LEXER_START" `TS.isPrefixOf`) . lineToText $ l

updateErrsOuts :: [Line] -> [Line] -> Either Line Line -> ([Line], [Line])
updateErrsOuts errs outs (Left e) = (e:errs, outs)
updateErrsOuts errs outs (Right o) = (errs, o:outs)

tigerFiles :: FilePath -> Shell FilePath
tigerFiles = find (suffix ".tig")

outFileName :: Text -> FilePath -> FilePath
outFileName ext inp = textToPath ("_out/" `TS.append` inpFName `TS.append` ext)
  where
    inpFName = pathToText . filename $ inp
{-
--       Hard-coded paths
-}

baseDir :: String
baseDir = "/home/ulysses/c/cs6410-compilers-TA"

submDir :: FilePath
submDir = decodeString $
  baseDir ++ "/submissions/a1/assignment_1492_sample"

testDir :: FilePath
testDir = decodeString $
  baseDir ++ "/mine/tiger-testcases"

testsShouldLexDir :: FilePath
testsShouldLexDir = testDir </> "lex/should_lex"

testZero :: Text
testZero = TS.pack $
  baseDir ++ "/mine/tiger-testcases/test.tig"

runScript :: FilePath
runScript = decodeString $
  baseDir ++ "/mine/tiger-my/chap2/run.sml"

{-
--       I hate string-like type conversions
-}

pathToLine :: FilePath -> Line
pathToLine = fromJust . textToLine . fromRight "" . toText

stringToLine :: String -> Line
stringToLine = fromJust . textToLine . TS.pack

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
