-- {-# LANGUAGE QuasiQuotes #-}
-- Give a haskell syntax like text like:
-- a = -4.6
-- b = '3'
-- c = "hello world"
-- d = if a > 4 then e else f
-- e f = f g "rtg"
-- e f g
--   | f g = m "hrt"
--   | g f == 8 = m 'r'
--   | otherwise = m 5.6
--   where f g h = let g = h (-5)
--                     m = n 
--                 in  c "12"
--         h i = j
-- Write a lexical analysis, parse it like:
-- [Indent 0,Identifier "a",Sysbol Assignment,Literal (LDouble (-4.6)),Indent 0,Identifier "b",Sysbol Assignment,Literal (LChar '3'),Indent 0,Identifier "c",Sysbol Assignment,Literal (LString "hello world"),Indent 0,Identifier "d",Sysbol Assignment,KeyWord If,Identifier "a",Operator Great,Literal (LInt 4),KeyWord Then,Identifier "e",KeyWord Else,Identifier "f",Indent 0,Identifier "e",Identifier "f",Sysbol Assignment,Identifier "f",Identifier "g",Literal (LString "rtg"),Indent 0,Identifier "e",Identifier "f",Identifier "g",Indent 2,Sysbol Vertical,Identifier "f",Identifier "g",Sysbol Assignment,Identifier "m",Literal (LString "hrt"),Indent 2,Sysbol Vertical,Identifier "g",Identifier "f",Operator Equal,Literal (LInt 8),Sysbol Assignment,Identifier "m",Literal (LChar 'r'),Indent 2,Sysbol Vertical,KeyWord Otherwise,Sysbol Assignment,Identifier "m",Literal (LDouble 5.6),Indent 2,KeyWord Where,Identifier "f",Identifier "g",Identifier "h",Sysbol Assignment,KeyWord Let,Identifier "g",Sysbol Assignment,Identifier "h",Sysbol LeftParen,Literal (LInt (-5)),Sysbol RightParen,Indent 20,Identifier "m",Sysbol Assignment,Identifier "n",Indent 16,KeyWord In,Identifier "c",Literal (LString "12"),Indent 8,Identifier "h",Identifier "i",Sysbol Assignment,Identifier "j",Indent 0]

module LexAnalysis
  (
    lexAnalysis
  ) where

import Control.Monad.State
import Control.Applicative
import Prelude hiding ( lex )
import qualified Data.Map as Map
-- import Text.RawString.QQ

-- lexText = [r|a = -4.6
-- b = '3'
-- c = "hello world"
-- d = if a > 4 then e else f
-- e f = f g "rtg"
-- e f g
--   | f g = m "hrt"
--   | g f == 8 = m 'r'
--   | otherwise = m 5.6
--   where f g h = let g = h (-5)
--                     m = n 
--                 in  c "12"
--         h i = j
-- |]

newtype Parser a = Parser (String -> StateT (Row, Col) (Either String) (a, String))

lexAnalysis :: String -> Either String [Lex]
lexAnalysis = runParser lexes

runParser :: Parser a -> String -> Either String a
runParser (Parser parser) s = do
  ((a, s'), (row, col)) <- e 
  if s' == "" 
    then return a 
    else Left $ "Can't parse all input, end at row " <> show row <> ", col " <> show col 
  where e = runStateT (parser $ '\n':s) (0, 0)

oneChar :: Parser Char
oneChar = Parser $ \s -> 
  case s of
    "" -> do
      (row, col) <- get
      lift . Left $ "Expecr a Char, but get an empty input at row " <> show row <> ", col " <> show col 
    (x:xs) -> do
      (row, col) <- get
      put $ if x == '\n' then (row + 1, 0) else (row, col + 1)
      return (x, xs)

rowCol :: Parser (Row, Col)
rowCol = Parser $ \s -> do
  (row, col) <- get
  return ((row, col), s)

char :: Char -> Parser Char
char c = do
  c' <- oneChar
  if c == c' 
    then return c 
    else do
      (row, col) <- rowCol
      fail $ "Expect a char " <> show c <> ", at row " <> show row <> ", col " <> show col 

charSatisfy :: (Char -> Bool) -> Parser Char
charSatisfy p = do
  c <- oneChar
  if p c
    then return c 
    else do
      (row, col) <- rowCol
      fail $ "get a unexpected char " <> show c <> ", at row " <> show row <> ", col " <> show col 

lookAheadChar :: Char -> Parser Bool
lookAheadChar c = Parser $ \s -> 
  case s of
    "" -> return (False, "")
    (x:xs) -> return (x == c, s)

string :: String -> Parser String
string "" = return ""
string (x:xs) = liftM2 (:) (char x) (string xs)

newline :: Parser Char
newline = char '\n'

space :: Parser Char
space = char ' '

spaces :: Parser [Char]
spaces = many $ char ' '

instance Monad Parser where
  return a = Parser $ \s -> lift . return $ (a, s)
  fail e = Parser $ \s -> lift . Left $ e
  (Parser parser) >>= f = Parser $ \s -> do
    (a, s') <- parser s
    let Parser parser' = f a
    parser' s'

instance Applicative Parser where
  pure = return
  Parser f <*> Parser parser = Parser $ \s -> do
    (a, s') <- parser s
    (f', s'') <- f s' 
    return (f' a, s'')
  parser <* parser' = do
    a <- parser
    parser'
    return a

instance Functor Parser where
  f `fmap` Parser parser = Parser $ \s -> do
    (a, s') <- parser s
    return (f a, s')

instance Alternative Parser where
  empty = fail "empty at Alternative"
  Parser parser <|> Parser parser' = Parser $ \s -> parser s <|> parser' s
  some parser = liftM2 (:) parser (many parser)
  many parser = liftM2 (:) parser (many parser) <|> return []

type Row = Int
type Col = Int

data Lex = Identifier String
         | Indent Int
         | Sysbol Sysbol
         | Operator Operator
         | Literal Literal
         | KeyWord KeyWord deriving (Show, Eq, Ord)

lex :: Parser Lex
lex = indent
    <|> (symbol >>= return . Sysbol) <* spaces
    <|> (operator >>= return . Operator) <* spaces
    <|> (literal >>= return . Literal) <* (some space <|> (lookAheadChar '\n' >> return ""))
    <|> identifier <* (some space <|> (lookAheadChar '\n' >> return ""))

lexes :: Parser [Lex]
lexes = do
  lexes' <- many lex
  return $ idToKeywords lexes'

idToKeywords :: [Lex] -> [Lex]
idToKeywords [] = []
idToKeywords (x:xs) = maybe x id (keywords Map.!? x): idToKeywords xs

identifier :: Parser Lex
identifier = do
  c <- charSatisfy (`elem` '_':['a' .. 'z'] <> ['A' .. 'Z'])
  cs <- many $ charSatisfy (`elem` '_':['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])
  return $ Identifier (c:cs)

indent :: Parser Lex
indent = do
  char '\n'
  cs <- many space
  return $ Indent (length cs)

data Sysbol = Assignment
            | Vertical
            | LeftParen
            | RightParen deriving (Show, Eq, Ord)

symbol :: Parser Sysbol
symbol = (do
  char '=' 
  yc <- lookAheadChar '='
  guard $ not yc
  return Assignment)
  <|> (char '|' >> return Vertical)
  <|> (char '(' >> return LeftParen)
  <|> (char ')' >> return RightParen)

data Operator = Great
              | Equal deriving (Show, Eq, Ord)

operator :: Parser Operator
operator = (char '>' >> return Great)
         <|> (string "==" >> return Equal)

data Literal = LInt Int
             | LDouble Double
             | LChar Char
             | LString String deriving (Show, Eq, Ord)

literal :: Parser Literal
literal = (do
  cs <- lDigit
  return $ if '.' `elem` cs
          then LDouble (read cs)
          else LInt (read cs) ) 
  <|> lChar <|> lString

lDigit :: Parser String
lDigit = do
  yc <- lookAheadChar '-'
  prefix <- if yc then string "-" else return ""
  cs <- some (charSatisfy (`elem` '.':['0' .. '9']))
  return $ prefix ++ cs

lChar :: Parser Literal
lChar = do
  char '\''
  c <- oneChar
  char '\''
  return $ LChar c

lString :: Parser Literal
lString = do
  char '"'
  cs <- many $ charSatisfy (/= '"')
  char '"'
  return $ LString cs

data KeyWord = If
             | Then
             | Else
             | Otherwise
             | Where
             | Let
             | In deriving (Show, Eq, Ord)

keywords :: Map.Map Lex Lex
keywords = Map.fromList [ (Identifier "if", KeyWord If)
                        , (Identifier "then", KeyWord Then)
                        , (Identifier "else", KeyWord Else)
                        , (Identifier "otherwise", KeyWord Otherwise)
                        , (Identifier "where", KeyWord Where)
                        , (Identifier "let", KeyWord Let)
                        , (Identifier "in", KeyWord In) ]
