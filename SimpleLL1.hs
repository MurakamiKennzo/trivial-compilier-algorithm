-- ll1 simple that doesn't consider empty string

module SimpleLL1
  (
    ll1Table
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

firsts :: CFG -> FirstSets
firsts cfg = let notTerminateSymbols = map fst cfg
                 firstSets = foldr (flip Map.insert mempty) mempty notTerminateSymbols
                 firstSets' = firsts' firstSets cfg
              in firsts'' firstSets' firstSets cfg

firsts' :: FirstSets -> CFG -> FirstSets
firsts' firstSets [] = firstSets
firsts' firstSets ((notTerminateSymbol, (Terminate symbol):_):restCfg) = firsts' (Map.adjust (Set.insert symbol) notTerminateSymbol firstSets) restCfg
firsts' firstSets ((notTerminateSymbol, (NotTerminate symbol):_):restCfg) = firsts' (Map.adjust (Set.union (firstSets Map.! symbol)) notTerminateSymbol firstSets) restCfg

firsts'' :: FirstSets -> FirstSets -> CFG -> FirstSets
firsts'' firstSets firstSets' cfg
  | firstSets == firstSets' = firstSets
  | otherwise = firsts'' (firsts' firstSets cfg) firstSets cfg

firsts_s :: CFG -> FirstSets -> SentenceFirstSets
firsts_s [] _ = mempty
firsts_s ((notTerminateSymbol, symbols@((Terminate symbol):_)):restCfg) firstSets = Map.insert symbols (Set.singleton symbol) $ firsts_s restCfg firstSets
firsts_s ((notTerminateSymbol, symbols@((NotTerminate symbol):_)):restCfg) firstSets = Map.insert symbols (firstSets Map.! symbol) $ firsts_s restCfg firstSets

ll1Table :: CFG -> LL1Table
ll1Table cfg = let table = ll1Table' cfg
               in  LL1Table cfg table

ll1Table' :: CFG -> Table
ll1Table' cfg = ll1Table'' (zip [0 ..] cfg) sentenceFirstSets
  where firstSets = firsts cfg
        sentenceFirstSets = firsts_s cfg firstSets
        ll1Table'' :: [(Int, CFGItem)] -> SentenceFirstSets -> Table
        ll1Table'' [] _ = mempty
        ll1Table'' ((n, (notTerminateSymbol, symbols)):xs) sentenceFirstSets = Map.insertWith Map.union notTerminateSymbol (Set.foldr (flip Map.insert n) mempty $ sentenceFirstSets Map.! symbols) $ ll1Table'' xs sentenceFirstSets

data LL1Table = LL1Table { cfg :: CFG
                         , table :: Table } deriving (Show)

type Table = Map.Map NotTerminateSymbol (Map.Map TerminateSymbol Int)

type FirstSets = Map.Map NotTerminateSymbol TerminateSymbols
type SentenceFirstSets = Map.Map [Symbol] TerminateSymbols

type TerminateSymbols = Set.Set TerminateSymbol

type CFGItem = (NotTerminateSymbol, [Symbol])
type CFG = [CFGItem]

type NotTerminateSymbol = String
type TerminateSymbol = String

data Symbol = NotTerminate NotTerminateSymbol
            | Terminate TerminateSymbol deriving (Show, Eq, Ord)

cfg1 :: CFG
cfg1 = [ ("S", [NotTerminate "N", NotTerminate "V", NotTerminate "N"])
       , ("N", [Terminate "s"])
       , ("N", [Terminate "t"])
       , ("N", [Terminate "g"])
       , ("N", [Terminate "w"])
       , ("V", [Terminate "e"])
       , ("V", [Terminate "d"]) ]
