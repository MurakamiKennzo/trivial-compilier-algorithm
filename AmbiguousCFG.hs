-- this is an example that show ambiguousCFG:
-- E -> Num
--   | E + E
--   | E * E
-- Num -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- if you have a sentence 1 + 2 + 3
--   start with E + E, get a parse tree:
--     NotTerminate "E" :-> [NotTerminate "E" :-> [NotTerminate "Num" :-> [Terminate "1" :-> []]],Terminate "+" :-> [],NotTerminate "E" :-> [NotTerminate "E" :-> [NotTerminate "Num" :-> [Terminate "2" :-> []]],Terminate "*" :-> [],NotTerminate "E" :-> [NotTerminate "Num" :-> [Terminate "3" :-> []]]]])
--   start with E * E, get a parse tree:
--     NotTerminate "E" :-> [NotTerminate "E" :-> [NotTerminate "E" :-> [NotTerminate "Num" :-> [Terminate "1" :-> []]],Terminate "+" :-> [],NotTerminate "E" :-> [NotTerminate "Num" :-> [Terminate "2" :-> []]]],Terminate "*" :-> [],NotTerminate "E" :-> [NotTerminate "Num" :-> [Terminate "3" :-> []]]]

module AmbiguousCFG
  (
    parseTree
  ) where

import qualified Data.Map as Map
import Debug.Trace

parseTree :: ParseOrders -> Symbol -> CFG -> Maybe ParseTree
parseTree None symbol _ = return $ symbol :-> []
parseTree _ (Terminate _) _ = Nothing
parseTree (Single n) symbol cfg = do
  rules <- Map.lookup symbol cfg
  let (SingleSymbol symbol') = rules !! n
  return $ symbol :-> [symbol' :-> []]
parseTree (Multiple (n, parseOrders)) symbol cfg = do
  rules <- Map.lookup symbol cfg
  let (rule) = rules !! n
  ruleNodes <- buildRuleNodes parseOrders rule
  nodes <- sequence . map (\(parseOrders', rule') -> traceShow (parseOrders', rule') $ parseTree parseOrders' rule' cfg) $ ruleNodes
  return $ symbol :-> nodes

buildRuleNodes :: [ParseOrders] -> ListSymbol -> Maybe [(ParseOrders, Symbol)]
buildRuleNodes [] _ = Nothing
buildRuleNodes (x:xs) (SingleSymbol symbol) = return [(x, symbol)]
buildRuleNodes (x:xs) (symbol@(Terminate _) :-: symbolList) = fmap ((None, symbol):) $ buildRuleNodes (x:xs) symbolList
buildRuleNodes (x:xs) (symbol@(NotTerminate _) :-: symbolList) = fmap ((x, symbol):) $ buildRuleNodes xs symbolList

type CFG = Map.Map Symbol [ ListSymbol ]

data ParseTree = Empty
               | Symbol :-> [ ParseTree ] deriving (Show)

data ParseOrders = None
                 | Single Int
                 | Multiple (Int, [ParseOrders]) deriving (Show)

infixr 5 :-:
data ListSymbol = SingleSymbol Symbol
                | Symbol :-: ListSymbol deriving (Show)

data Symbol = Terminate String
            | NotTerminate String deriving (Show, Eq, Ord)

expr :: CFG
expr = Map.fromList [ (NotTerminate "E", [ SingleSymbol $ NotTerminate "Num"
                                         , NotTerminate "E" :-: Terminate "+" :-: (SingleSymbol $ NotTerminate "E") 
                                         , NotTerminate "E" :-: Terminate "*" :-: (SingleSymbol $ NotTerminate "E")])
                    , (NotTerminate "Num", map (SingleSymbol . Terminate . return) ['0' .. '9']) ]

parseOrders :: ParseOrders
parseOrders = Multiple (1, [ Multiple (0, [ Single 1 ])
                           , Multiple (2, [ Multiple (0, [ Single 2 ]) 
                                          , Multiple (0, [ Single 3 ]) ]) ])
parseOrders' :: ParseOrders
parseOrders' = Multiple (2, [ Multiple (1, [ Multiple (0, [ Single 1 ])
                                           , Multiple (0, [ Single 2 ]) ])
                            , Multiple (0, [ Single 3 ]) ])                        
