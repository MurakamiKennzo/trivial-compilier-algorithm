module ParseExpr
  (
    parseExpr
  ) where

import Control.Arrow ( first )
import Control.Applicative ( Alternative ( (<|>)
                                         , many
                                         , some
                                         , empty ) )

parseExpr :: String -> IO ()
parseExpr string = case run string e of
                      Left error -> putStrLn error
                      _ -> putStrLn "succeed"

run :: String -> ParseSymbol a -> Either Error a
run string parseSymbol = case getParseSymbol parseSymbol string of
  Right (a, string') -> if null string' then return a else Left $ "not full parsed, left \"" <> string' <> "\""
  Left a -> Left a

e :: ParseSymbol ()
e = do
  t
  many addT
  return ()

t :: ParseSymbol ()
t = do
  f
  many multiplyF
  return ()

addT :: ParseSymbol ()
addT = do
  symbol "+"
  t
  return ()

f :: ParseSymbol ()
f = do
  some (satisfy (`elem` ['0' .. '9']))
  return ()

multiplyF :: ParseSymbol ()
multiplyF = do
  symbol "*"
  f
  return ()

symbol :: String -> ParseSymbol ()
symbol "" = return ()
symbol (x:xs) = do
  satisfy (== x)
  symbol xs

satisfy :: (Char -> Bool) -> ParseSymbol ()
satisfy pred = do
  c <- oneChar
  if pred c then return () else fail "one char satisfy error"

oneChar :: ParseSymbol Char
oneChar = ParseSymbol $ \string -> case string of
  "" -> Left "empty String, but expect a char"
  (x:xs) -> return (x, xs)

newtype ParseSymbol a = ParseSymbol { getParseSymbol :: String -> Either Error (a, String) }

type Error = String

instance Functor ParseSymbol where
  f `fmap` parseSymbol = ParseSymbol $ \string -> fmap (first f) $ getParseSymbol parseSymbol string

instance Applicative ParseSymbol where
  pure a = ParseSymbol $ \string -> return (a, string)
  parseSymbol <*> parseSymbol' = ParseSymbol $ \string -> do
    (a, string') <- getParseSymbol parseSymbol' string
    (f, string'') <- getParseSymbol parseSymbol string'
    return (f a, string'')

instance Monad ParseSymbol where
  return = pure
  fail string = ParseSymbol $ \_ -> Left string
  parseSymbol >>= f = ParseSymbol $ \string -> do
    (a, string') <- getParseSymbol parseSymbol string
    (b, string'') <- getParseSymbol (f a) string'
    return (b, string'')

instance Alternative ParseSymbol where
  empty = fail "not have implement empty"
  parseSymbol <|> parseSymbol' = ParseSymbol $ \string ->
    case getParseSymbol parseSymbol string of
      Left _ -> getParseSymbol parseSymbol' string
      a -> a
  many v = (do
    x <- v
    xs <- many v
    return (x:xs)) <|> pure []
  some v = do
    x <- v
    xs <- many v
    return (x:xs)
