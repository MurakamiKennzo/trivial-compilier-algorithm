-- write a AST that compile:
-- E -> n
--   | ( E )
--   | E + E

-- and the sentence is 1 + (2 + (3 + ( 4 + 5 )))

-- Note: the lro isn't the right way.

module AST
  (
    ast
  ) where

import Data.Char ( isDigit
                 , isSpace )
import Debug.Trace

tokens :: String -> Either TokenError [Token]
tokens [] = return []
tokens string = do
  let (currentTokenString@(lChar:currentTokenString'), restTokenString) = break isSpace . dropWhile isSpace $ string
      isSpecialString = not . isDigit $ lChar
  token' <- if isSpecialString then token [lChar] else token currentTokenString
  tokens' <- tokens $ (if isSpecialString then currentTokenString' else []) <> restTokenString
  return (token':tokens')
  
  where token :: String -> Either TokenError Token
        token "(" = return LeftBracket
        token ")" = return RightBracket
        token "+" = return Add
        token s
          | isNumber = return $ if null b then TInt $ read s else TDouble $ read s
          | otherwise = Left $ "unexpected string " <> show s
          where (a, b) = break (== '.') s
                b' = if null b then b else tail b
                isNumber = all isDigit $ a <> b'

type TokenError = String

data Token = TInt Int
           | TDouble Double
           | LeftBracket
           | RightBracket
           | Add
           | EOF deriving (Show)

ast :: [Token] -> Either SyntaxError Expr
ast tokens = ast' [(NotTerminate Start, Nothing, 0)] $ tokens <> [EOF]

ast' :: [StackItem] -> [Token] -> Either SyntaxError Expr
ast' _ [] = Left $ "need more tokens"
ast' [] _ = Left $ "syntax parser error"
ast' (stackItem:stack) (token:tokens) = do
  let (_, value, state) = traceShow (stackItem:stack) $ stackItem
  action <- actionTable state token
  case action of
    Accept -> case value of
                Just ast -> return ast
                _ -> Left "no parse value"
    Shift state' -> ast' ((Terminate token, return $ buildToken token, state'):stackItem:stack) tokens
    Reduce (notTerminateSymbol, n) -> let (preStack, sufStack@(topItem:_)) = splitAt n (stackItem:stack)
                                          value = buildValue notTerminateSymbol $ reverse preStack
                                          (_, _, state'') = topItem
                                      in  do
                                            state' <- gotoTable state'' notTerminateSymbol
                                            ast' ((NotTerminate notTerminateSymbol, value, state'):sufStack) (token:tokens)

buildToken :: Token -> Expr
buildToken (TInt n) = EInt n
buildToken (TDouble n) = EDouble n
buildToken token = EToken token

buildValue :: NotTerminateSymbol -> [StackItem] -> Maybe Expr
buildValue E [(_, Just ast, _)] = return ast
buildValue E [(Terminate LeftBracket, _, _), (_, Just ast, _), (Terminate RightBracket, _, _)] = return ast
buildValue E [(_, Just ast, _), (Terminate Add, _, _), (_, Just ast', _)] = return $ EAdd ast ast'
buildValue _ _ = Nothing

type StackItem = (Symbol, Maybe Expr, State)
type SyntaxError = String

data Symbol = NotTerminate NotTerminateSymbol
            | Terminate Token deriving (Show)

data Expr = EAdd Expr Expr
          | EToken Token
          | EInt Int
          | EDouble Double deriving (Show)

data NotTerminateSymbol = Start
                        | E deriving (Show)

type ActionTable = State -> Token -> Either SyntaxError Action
type GotoTable = State -> NotTerminateSymbol -> Either SyntaxError State
type State = Int

data Action = Shift State
            | Reduce (NotTerminateSymbol, Int)
            | Accept

actionTable :: ActionTable
actionTable 0 (TInt _) = return $ Shift 1
actionTable 0 (TDouble _) = return $ Shift 1
actionTable 0 LeftBracket = return $ Shift 4
actionTable 1 _ = return $ Reduce (E, 1)
actionTable 2 EOF = return Accept
actionTable 2 Add = return $ Shift 6
actionTable 4 LeftBracket = return $ Shift 4
actionTable 4 (TInt _) = return $ Shift 1
actionTable 4 (TDouble _) = return $ Shift 1
actionTable 5 Add = return $ Shift 6
actionTable 5 RightBracket = return $ Shift 7
actionTable 6 LeftBracket = return $ Shift 4
actionTable 6 (TInt _) = return $ Shift 1
actionTable 6 (TDouble _) = return $ Shift 1
actionTable 7 _ = return $ Reduce (E, 3)
actionTable 8 Add = return $ Shift 6
actionTable 8 _ = return $ Reduce (E, 3)
actionTable _ token = Left $ "unexpected token " <> show token

gotoTable :: GotoTable
gotoTable 0 E = return 2
gotoTable 4 E = return 5
gotoTable 6 E = return 8
gotoTable _ _ = Left $ "syntax error"
