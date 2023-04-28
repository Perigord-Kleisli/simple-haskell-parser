{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Applicative (Alternative (some))
import Control.Monad
import Data.Functor.Identity
import Text.Parsec hiding (parseTest)
import Text.Parsec.Expr (OperatorTable)
import Text.Parsec.Pos
import qualified Text.Parsec.String as PS
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Prim as PP

data IndentMode
  = NoIndent
  | Block
  | LineFold
  deriving (Eq)

data IndentState = IndentState
  { indentMode :: IndentMode,
    indentPos :: SourcePos
  }

type IndentParser tok st a = PP.GenParser tok (st, IndentState) a

type IndentCharParser st a = IndentParser Char st a

getState :: IndentParser tok st st
getState = fmap fst PP.getState

getIndentState :: IndentParser tok st IndentState
getIndentState = fmap snd PP.getState

getIndentMode :: IndentParser tok st IndentMode
getIndentMode = fmap indentMode getIndentState

getIndentPos :: IndentParser tok st SourcePos
getIndentPos = fmap indentPos getIndentState

setState :: st -> IndentParser tok st ()
setState st = do
  indst <- getIndentState
  PP.setState (st, indst)

setIndentPos :: SourcePos -> IndentParser tok st ()
setIndentPos sp = do
  indst <- getIndentState
  setIndentState $ indst {indentPos = sp}

setIndentMode :: IndentMode -> IndentParser tok st ()
setIndentMode indm = do
  indst <- getIndentState
  setIndentState $ indst {indentMode = indm}

setIndentState :: IndentState -> IndentParser tok st ()
setIndentState indst = do
  st <- Main.getState
  PP.setState (st, indst)

column :: PP.GenParser tok st Column
column = fmap sourceColumn getPosition

indentColumn :: IndentParser tok st Column
indentColumn = fmap sourceColumn getIndentPos

indentGT :: SourcePos -> SourcePos -> Bool
indentGT pos1 pos2 = sourceColumn pos1 > sourceColumn pos2

indentGE pos1 pos2 = sourceColumn pos1 >= sourceColumn pos2

indentEq pos1 pos2 = sourceColumn pos1 == sourceColumn pos2

indentParser :: IndentParser tok st a -> IndentParser tok st a
indentParser p = do
  indMode <- getIndentMode
  case indMode of
    NoIndent -> p
    Block -> inBlockMode p
    LineFold -> inLineFoldMode p
  where
    inBlockMode p = do
      curCol <- column
      indCol <- indentColumn
      if curCol >= indCol then p else PP.pzero

    inLineFoldMode p = do
      curPos <- getPosition
      oldPos <- getIndentPos
      if curPos
        `indentGT` oldPos
        || curPos
        == oldPos
        then p
        else PP.pzero

withIndentMode :: IndentParser tok st a -> IndentMode -> IndentParser tok st a
withIndentMode p indm = do
  setIndentMode indm
  p

saveIndent :: IndentParser tok st a -> IndentParser tok st a
saveIndent p = do
  indMode <- getIndentMode
  indPos <- getIndentPos
  x <- p
  setIndentMode indMode
  setIndentPos indPos
  return x

nextPos :: PP.GenParser tok st SourcePos
nextPos = do
  pos <- getPosition
  return (pos `incSourceColumn` 1)

noIndent :: IndentParser tok st a -> IndentParser tok st a
noIndent p = saveIndent (p `withIndentMode` NoIndent)

block :: IndentParser tok st a -> IndentParser tok st a
block p = saveIndent $ do
  setPosBlock
  x <- p `withIndentMode` Block
  eobReached
  return x

lineFold :: IndentParser tok st a -> IndentParser tok st a
lineFold p = saveIndent $ do
  setPosLineFold
  x <- p `withIndentMode` LineFold
  eolfReached
  return x

isEmptyBlock :: IndentParser tok st Bool
isEmptyBlock = do
  indm <- getIndentMode
  case indm of
    NoIndent -> return False
    _ -> do
      curCol <- column
      indCol <- indentColumn
      return (curCol <= indCol)

isEmptyLineFold :: IndentParser tok st Bool
isEmptyLineFold = do
  indm <- getIndentMode
  case indm of
    NoIndent -> return False
    Block -> do
      curCol <- column
      indCol <- indentColumn
      return (curCol < indCol)
    LineFold -> do
      curCol <- column
      indCol <- indentColumn
      return (curCol <= indCol)

setPosBlock :: IndentParser tok st ()
setPosBlock = do
  test <- isEmptyBlock
  pos <- if test then nextPos else getPosition
  setIndentPos pos

setPosLineFold :: IndentParser tok st ()
setPosLineFold = do
  test <- isEmptyLineFold
  pos <- if test then nextPos else getPosition
  setIndentPos pos

endOfInput :: IndentParser tok st ()
endOfInput = do
  ins <- getInput
  case ins of
    [] -> return ()
    _ -> PP.pzero

eobReached :: IndentParser tok st ()
eobReached = do
  indCol <- indentColumn
  col <- column
  when (indCol <= col) $ endOfInput <|> prematureEnd
  where
    prematureEnd = do
      indPos <- getIndentPos
      pos <- getPosition
      fail
        ( "premature block termination "
            ++ "started at "
            ++ show indPos
            ++ " and ended at "
            ++ show pos
        )

eolfReached :: IndentParser tok st ()
eolfReached = do
  indCol <- indentColumn
  col <- column
  when (indCol < col) $ endOfInput <|> prematureEnd
  where
    prematureEnd = do
      indPos <- getIndentPos
      pos <- getPosition
      fail
        ( "premature line fold termination "
            ++ "started at "
            ++ show indPos
            ++ " and ended at "
            ++ show pos
        )

newtype ParserState = ParserState {operators :: OperatorTable String ParserState Identity Expr}

type Parser a = IndentCharParser ParserState a

data Expr

singleLineComment :: Parser ()
singleLineComment =
  do
    void $ try (string "--")
    skipMany (satisfy (/= '\n'))
    return ()

multilineComment :: Parser ()
multilineComment = void
  do
    void $ try (string "{-")
    inComment
  where
    inComment =
      choice
        [ void $ try (string "-}"),
          skipMany1 (noneOf "{-}") *> inComment,
          oneOf "{-}" *> inComment
        ]

whitespace :: Parser ()
whitespace = skipMany (skipMany1 (char ' ' <|> char '\t') <|> singleLineComment <|> multilineComment)

-- | Like `whitespace` but with newlines
whitespacen :: Parser ()
whitespacen = skipMany (skipMany1 space <|> singleLineComment <|> multilineComment)

lexeme :: Parser a -> Parser a
lexeme = indentParser . (<* whitespacen)

type Identifier = String

identifier :: Parser Identifier
identifier = indentParser named
  where
    named = (:) <$> (lower <|> char '_') <*> many (alphaNum <|> char '_')

symbol :: String -> Parser String
symbol s = indentParser $ lexeme (string s)

doNotation :: Parser [String]
doNotation = symbol "do" *> block (many (string "a" <* whitespacen))

res = parseTest doNotation 
    $ unlines ["do a"
              ,"   a"
              ]


main :: IO ()
main = putStrLn "WIP: Parser"

runParser ::
  IndentParser tok st a ->
  st ->
  IndentMode ->
  SourceName ->
  [tok] ->
  Either ParseError a
runParser p st imode sname = PP.runParser p (st, istate) sname
  where
    istate =
      IndentState
        { indentPos = initialPos sname,
          indentMode = imode
        }

parse ::
  IndentParser tok () a ->
  SourceName ->
  [tok] ->
  Either ParseError a
parse p = Main.runParser p () NoIndent

parseTest ::
  Show a =>
  IndentParser tok ParserState a ->
  [tok] ->
  IO ()
parseTest p input = case result of
  Left err -> do putStr "Error"; print err
  Right a -> do print a
  where
    result = Main.runParser p (ParserState []) NoIndent "" input

-- runParserTest :: Show a => Parser a -> String -> IO ()
-- runParserTest p s = case runParser p (ParserState []) "test" s of
--   Left e -> print e
--   Right x -> print x
