-- write a syntax-directed translation that:
-- S -> E
-- E -> E + E
--   | n

module SDT
  (
    sdt
  ) where

sdt :: Tokens -> Either SyntaxError SDT
sdt tokens = sdt' [(NotTerminate Start, Nothing, 0)] (tokens <> [EOF])

sdt' :: Stack -> Tokens -> Either SyntaxError SDT
sdt' _ [] = Left "syntax error" 
sdt' [] _ = Left "syntax error" 
sdt' (stackItem:stack) (token:tokens) = do
  let (_, value, state) = stackItem
  action <- actions state token
  case action of
    Accept -> case value of
                Just sdt -> return sdt
                _ -> Left "syntax error"
    Shift state' -> sdt' ((Terminate token, return $ buildToken token, state'):stackItem:stack) tokens
    Reduce (notTerminateSymbol, n) -> let (preStack, sufStack@(topItem:_)) = splitAt n (stackItem:stack)
                                          value = buildValue notTerminateSymbol $ reverse preStack
                                          (_, _, state'') = topItem
                                      in  do
                                            state' <- gotos state'' notTerminateSymbol
                                            sdt' ((NotTerminate notTerminateSymbol, value, state'):sufStack) (token:tokens)

buildToken :: TerminateSymbol -> SDT
buildToken Add = AddSymbol
buildToken (Number s) = N s

buildValue :: NotTerminateSymbol -> Stack -> Maybe SDT
buildValue E [(_, Just sdt, _)] = return sdt
buildValue E [(_, Just sdt, _), _, (_, Just sdt', _)] = return $ AddExpr sdt sdt'
buildValue _ _ = Nothing

data Symbol = NotTerminate NotTerminateSymbol
            | Terminate TerminateSymbol
type State = Int
type Value = Maybe SDT
type Stack = [(Symbol, Value, State)]
type Tokens = [TerminateSymbol]

data SDT = AddExpr SDT SDT
         | N String
         | AddSymbol deriving (Show)

data NotTerminateSymbol = Start
                        | E
data TerminateSymbol = Add
                     | Number String
                     | EOF deriving (Show)
type SyntaxError = String
type ActionTable = State -> TerminateSymbol -> Either SyntaxError Action
type GotoTable = State -> NotTerminateSymbol -> Either SyntaxError State

data Action = Shift State
            | Reduce (NotTerminateSymbol, Int)
            | Accept

actions :: ActionTable
actions 0 (Number n) = return $ Shift 1
actions 1 _ = return $ Reduce (E, 1)
actions 2 Add = return $ Shift 3
actions 2 EOF = return $ Accept
actions 3 (Number n) = return $ Shift 1
actions 4 _ = return $ Reduce (E, 3)
actions _ token = Left $ "unexpected token: \"" <> show token <> "\""

gotos :: GotoTable
gotos 0 E = return 2
gotos 3 E = return 4
gotos _ _ = Left "syntax error"
