-- simple lr0 analysis that 
-- S' -> S \0
-- S -> x x T
-- T -> y

module LR0
  (
    lr0Analysis
  , lr0
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import Control.Arrow ( second )

lr0Analysis :: Tokens -> CFG -> Either SyntaxError ()
lr0Analysis sentences cfg = let lr0' = lr0 cfg
                           in  lr0Analysis' [LR0State 0, LR0Terminate "\0"] lr0' (sentences <> ["\0"])

lr0Analysis' :: LR0AnalysisStack -> LR0 -> Tokens -> Either SyntaxError ()
lr0Analysis' [] _ sentences = if null sentences then return () else Left $ "can't parse all input, left \"" <> show sentences <> "\""
lr0Analysis' (x:xs) _ [] = Left "syntax error"
lr0Analysis' s@(LR0State x:xs) lr0'@(LR0 _ actions gotos) (token: tokens) = 
  case action of
    Nothing -> Left $ "unexpected input \"" <> token <> "\""
    Just action' -> case action' of
                      Shift n -> let s' = LR0State n: LR0Terminate token: s in lr0Analysis' s' lr0' tokens
                      Reduce (notTerminateSymbol, symbols) -> let s'@(LR0State x':xs') = drop (2 * length symbols) s
                                                                  goto = gotos Map.!? x' >>= (Map.!? notTerminateSymbol)
                                                                  s'' = fmap ((:LR0NotTerminate notTerminateSymbol: s') . LR0State) . fmap (\(Goto s) -> s) $ goto
                                                              in  if s'' == Nothing then Left $ "syntax error 1" else lr0Analysis' (fromJust s'') lr0' (token:tokens)
                      Accept -> if null tokens then return () else Left $ "can't parse all input, left \"" <> show tokens <> "\""
  where action = actions Map.!? x >>= (Map.!? token)

type Tokens = [String]
type SyntaxError = String
type LR0AnalysisStack = [ LR0AnalysisStackItem ]
data LR0AnalysisStackItem = LR0NotTerminate NotTerminateSymbol
                          | LR0Terminate TerminateSymbol
                          | LR0State Int deriving (Show, Eq)

lr0 :: CFG -> LR0
lr0 cfg = let state = closure cfg $ Set.singleton ("S'", [Dot, Symbol $ NotTerminate "S", Symbol $ Terminate "\0"])
              notTerminateSymbols = ("S'":) $ map fst cfg
              terminateSymbols = ("\0":) . map extractSymbol . filter isTerminateSymbol . foldMap snd $ cfg
              (actions, gotos) = lr0' cfg [state] [state] (terminateSymbols, notTerminateSymbols) (mempty, mempty)
          in  LR0 cfg actions gotos

addReduceActions :: States -> [TerminateSymbol] -> ActionTable -> ActionTable
addReduceActions states terminateSymbols actions = foldr foldrFn actions states
  where foldrFn :: State -> ActionTable -> ActionTable
        foldrFn state actions = let n = fromJust $ state `elemIndex` states
                                    headItem = 0 `Set.elemAt` state
                                    cfgItem = flip second headItem $ map (\(Symbol s) -> s) . init
                                in  if (last $ snd headItem) == Dot && fst headItem /= "S'"
                                      then foldr (\terminateSymbol actions -> Map.insertWith Map.union n (Map.singleton terminateSymbol $ Reduce cfgItem) actions) actions terminateSymbols
                                      else actions

lr0' :: CFG -> States -> WorkList -> ([TerminateSymbol], [NotTerminateSymbol]) -> (ActionTable, GotoTable) ->  (ActionTable, GotoTable)
lr0' _ states [] (terminateSymbols, _) (actions, gotos) = (addReduceActions states terminateSymbols actions, gotos)
lr0' cfg states (state:worklist) (terminateSymbols, notTerminateSymbols) (actions, gotos) = 
  let (states', worklist', actions') = terminateSymbolsWalk cfg state terminateSymbols (states, worklist, actions)
      (states'', worklist'', gotos') = notTerminateSymbolsWalk cfg state notTerminateSymbols (states', worklist', gotos)
  in  lr0' cfg states'' worklist'' (terminateSymbols, notTerminateSymbols) (actions', gotos')

terminateSymbolsWalk :: CFG -> State -> [TerminateSymbol] -> (States, WorkList, ActionTable) ->  (States, WorkList, ActionTable)
terminateSymbolsWalk _ _ [] (states, worklist, actions) = (states, worklist, actions)
terminateSymbolsWalk cfg state (terminateSymbol:terminateSymbols) (states, worklist, actions) = 
  let state' = roadState cfg state (Terminate terminateSymbol)
      states' = if null state' || state' `elem` states then states else states <> [state']
      worklist' = if null state' || state' `elem` states then worklist else worklist <> [state']
      actions' = if null state' then actions else Map.insertWith Map.union (fromJust $ state `elemIndex` states') (Map.singleton terminateSymbol $ getAction state' states') actions
  in  terminateSymbolsWalk cfg state terminateSymbols (states', worklist', actions')

notTerminateSymbolsWalk :: CFG -> State -> [NotTerminateSymbol] -> (States, WorkList, GotoTable) ->  (States, WorkList, GotoTable)
notTerminateSymbolsWalk _ _ [] (states, worklist, gotos) = (states, worklist, gotos)
notTerminateSymbolsWalk cfg state (notTerminateSymbol:notTerminateSymbols) (states, worklist, gotos) = 
  let state' = roadState cfg state (NotTerminate notTerminateSymbol)
      states' = if null state' || state' `elem` states then states else states <> [state']
      worklist' = if null state' || state' `elem` states then worklist else worklist <> [state']
      gotos' = if null state' then gotos else Map.insertWith Map.union (fromJust $ state `elemIndex` states') (Map.singleton notTerminateSymbol $ Goto . fromJust $ state' `elemIndex` states') gotos
  in  notTerminateSymbolsWalk cfg state notTerminateSymbols (states', worklist', gotos')

roadState :: CFG -> State -> Symbol -> State
roadState cfg state symbol = closure cfg $ foldr foldrFn mempty state
  where foldrFn :: CFGItemDot -> State -> State
        foldrFn (notTerminateSymbol, symbolDots) state = let (starts, Dot:rest) = break (== Dot) symbolDots
                                   in  case rest of 
                                        [] -> state
                                        (x@(Symbol symbol'):xs) -> if symbol == symbol' then Set.insert (notTerminateSymbol, starts <> [x] <> [Dot] <> xs) state else state

getAction :: State -> States -> Action
getAction state states = let n = fromJust $ state `elemIndex` states
                             headItem = 0 `Set.elemAt` state
                         in  if (last $ snd headItem) == Dot && fst headItem == "S'"
                                then Accept
                                else Shift n

closure :: CFG -> State -> State
closure cfg state = let state' = closure' cfg state state
                    in  closure'' cfg state' state

closure' :: CFG -> State -> State -> State
closure' cfg originState state = Set.foldl foldlFn originState state
  where foldlFn :: State -> CFGItemDot -> State
        foldlFn state (notTerminateSymbol, symbolDots) = 
          let (_, Dot:rest) = break (== Dot) symbolDots
          in  case rest of
                [] -> state
                (Symbol x:_) -> if isTerminateSymbol x then state else let x' = extractSymbol x in Set.union (Set.fromList . map (\(notTerminateSymbol, symbols) -> (notTerminateSymbol, Dot:map Symbol symbols)) $ filter ((== x') . fst) cfg) state

closure'' :: CFG -> State -> State -> State
closure'' cfg state state'
  | state == state' = state
  | otherwise = closure'' cfg (closure' cfg state state) state

isTerminateSymbol :: Symbol -> Bool
isTerminateSymbol (Terminate _) = True
isTerminateSymbol _ = False

extractSymbol :: Symbol -> String
extractSymbol (Terminate s) = s
extractSymbol (NotTerminate s) = s

type State = Set.Set CFGItemDot
type States = [State]
type WorkList = [State]
type CFGItemDot = (NotTerminateSymbol, [SymbolDot])

data SymbolDot = Symbol Symbol
               | Dot deriving (Show, Eq, Ord)

data LR0 = LR0 { cfg :: CFG
               , action :: ActionTable
               , goto :: GotoTable } deriving (Show)

type ActionTable = Map.Map State' (Map.Map TerminateSymbol Action)
type GotoTable = Map.Map State' (Map.Map NotTerminateSymbol Goto)

type State' = Int
data Action = Shift State'
            | Reduce CFGItem
            | Accept deriving (Show)

newtype Goto = Goto State' deriving (Show)

type NotTerminateSymbol = String
type TerminateSymbol = String

type CFGItem = (NotTerminateSymbol, [Symbol])
type CFG = [CFGItem]

data Symbol = NotTerminate NotTerminateSymbol
            | Terminate TerminateSymbol deriving (Show, Eq, Ord)

cfg1 :: CFG
cfg1 = [ ("S", [ Terminate "x", Terminate "x", NotTerminate "T" ])
       , ("T", [Terminate "y"] ) ]
