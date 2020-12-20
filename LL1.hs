-- the complete ll1

module LL1
  (
    ll1
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

ll1 :: CFG -> LL1
ll1 cfg = let t = ll1Table cfg
          in  LL1 cfg t

ll1Table :: CFG -> LL1Table
ll1Table cfg = ll1Table' first_SSet (zip [0 ..] cfg) mempty
  where nullableSet = nullables cfg
        firstSet = firsts nullableSet cfg
        followSet = follows nullableSet firstSet cfg
        first_SSet = firsts_s nullableSet firstSet followSet cfg
        ll1Table' :: First_SSet -> [(Int, CFGItem)] -> LL1Table -> LL1Table
        ll1Table' _ [] t = t
        ll1Table' first_SSet ((n, (notTerminateSymbol, _)): xs) t = ll1Table' first_SSet xs t'
          where t' = Set.foldr 
                      (\terminateSymbol t -> Map.insertWith (Map.unionWith Set.union) notTerminateSymbol (Map.singleton terminateSymbol (Set.singleton n)) t)
                      t $ first_SSet Map.! n

data LL1 = LL1 { cfg :: CFG
               , table :: LL1Table } deriving (Show)
type LL1Table = Map.Map NotTerminateSymbol (Map.Map TerminateSymbol (Set.Set Int))

firsts_s :: NullableSet -> FirstSet -> FollowSet -> CFG -> First_SSet
firsts_s nullableSet firstSet followSet cfg = foldr firsts_s' mempty $ zip [0 ..] cfg
  where firsts_s' :: (Int, CFGItem) -> First_SSet -> First_SSet
        firsts_s' (n, (notTerminateSymbol, [])) first_SSet = Map.insertWith Set.union n (followSet Map.! notTerminateSymbol) first_SSet
        firsts_s' (n, (notTerminateSymbol, (Terminate symbol):symbols)) first_SSet
          | symbol == "" = firsts_s' (n, (notTerminateSymbol, symbols)) first_SSet
          | otherwise = Map.insertWith Set.union n (Set.singleton symbol) first_SSet
        firsts_s' (n, (notTerminateSymbol, (NotTerminate symbol):symbols)) first_SSet
          | symbol `Set.member` nullableSet = firsts_s' (n, (notTerminateSymbol, symbols)) first_SSet'
          | otherwise = first_SSet'
          where first_SSet' = Map.insertWith Set.union n (firstSet Map.! symbol) first_SSet

type First_SSet = Map.Map Int (Set.Set TerminateSymbol)

follows :: NullableSet -> FirstSet -> CFG -> FollowSet
follows nullableSet firstSet cfg = let followSet = foldr (flip Map.insert mempty) mempty (map fst cfg)
                                       followSet' = follows' nullableSet firstSet followSet cfg
                                   in  follows'' nullableSet firstSet followSet' followSet cfg

follows' :: NullableSet -> FirstSet -> FollowSet -> CFG -> FollowSet
follows' nullableSet firstSet followSet [] = followSet
follows' nullableSet firstSet followSet ((notTerminateSymbol, symbols):restCfg) = follows' nullableSet firstSet followSet' restCfg
  where followSet' = buildFollowSet nullableSet firstSet followSet (followSet Map.! notTerminateSymbol) (notTerminateSymbol, reverse symbols)
        buildFollowSet :: NullableSet -> FirstSet -> FollowSet -> Set.Set TerminateSymbol -> CFGItem -> FollowSet
        buildFollowSet nullableSet firstSet followSet terminateSymbols (notTerminateSymbol, []) = followSet
        buildFollowSet nullableSet firstSet followSet terminateSymbols (notTerminateSymbol, symbol:symbols) = 
          case symbol of
            (Terminate symbol') -> buildFollowSet nullableSet firstSet followSet (Set.singleton symbol') (notTerminateSymbol, symbols)
            (NotTerminate symbol') -> let followSet' = Map.insertWith Set.union symbol' terminateSymbols followSet
                                          terminateSymbols' = if symbol' `Set.member` nullableSet then terminateSymbols `Set.union` (firstSet Map.! symbol') else firstSet Map.! symbol'
                                      in  buildFollowSet nullableSet firstSet followSet' terminateSymbols' (notTerminateSymbol, symbols)

follows'' :: NullableSet -> FirstSet -> FollowSet -> FollowSet -> CFG -> FollowSet
follows'' nullableSet firstSet followSet followSet' cfg
  | followSet == followSet' = followSet
  | otherwise = follows'' nullableSet firstSet (follows' nullableSet firstSet followSet cfg) followSet cfg

type FollowSet = Map.Map NotTerminateSymbol (Set.Set TerminateSymbol)

firsts :: NullableSet -> CFG -> FirstSet
firsts nullableSet cfg = let firstSet = foldr (flip Map.insert mempty) mempty (map fst cfg)
                             firstSet' = firsts' nullableSet firstSet cfg
                         in  firsts'' nullableSet firstSet' firstSet cfg

firsts' :: NullableSet -> FirstSet -> CFG -> FirstSet
firsts' nullableSet firstSet [] = firstSet
firsts' nullableSet firstSet ((notTerminateSymbol, symbols):restCfg) = firsts' nullableSet firstSet' restCfg
  where firstSet' = fst $ foldl 
          (\(firstSet, b) symbol -> 
            if b 
              then (firstSet, b) 
              else if isNotTerminateSymbol symbol
                then let symbol' = extractNotTerminateSymbol symbol in if symbol' `Set.member` nullableSet then (Map.insertWith Set.union notTerminateSymbol (firstSet Map.! symbol') firstSet, b) else (firstSet, not b)
                  else let symbol' = extractTerminateSymbol symbol in if null symbol' then (firstSet, b) else (Map.insertWith Set.union notTerminateSymbol (Set.singleton symbol') firstSet, not b)) 
          (firstSet, False) 
          symbols

firsts'' :: NullableSet -> FirstSet -> FirstSet -> CFG -> FirstSet
firsts'' nullableSet firstSet firstSet' cfg
  | firstSet == firstSet' = firstSet
  | otherwise = firsts'' nullableSet (firsts' nullableSet firstSet cfg) firstSet cfg

type FirstSet = Map.Map NotTerminateSymbol (Set.Set TerminateSymbol)

nullables :: CFG -> NullableSet
nullables cfg = let nullableSet = mempty
                    nullableSet' = nullables' nullableSet cfg
                in  nullables'' nullableSet' nullableSet cfg

nullables' :: NullableSet -> CFG -> NullableSet
nullables' nullableSet [] = nullableSet
nullables' nullableSet ((notTerminateSymbol, symbols@(symbol:_)):restCfg) = nullables' newNullableSet restCfg
  where newNullableSet = case symbol of
                            (Terminate "") -> Set.insert notTerminateSymbol nullableSet
                            _ -> if all (`Set.member` nullableSet) . map extractNotTerminateSymbol . filter isNotTerminateSymbol $ symbols
                                    then Set.insert notTerminateSymbol nullableSet
                                    else nullableSet
       
extractNotTerminateSymbol :: Symbol -> NotTerminateSymbol
extractNotTerminateSymbol (NotTerminate s) = s 

extractTerminateSymbol :: Symbol -> TerminateSymbol
extractTerminateSymbol (Terminate s) = s 


isNotTerminateSymbol :: Symbol -> Bool
isNotTerminateSymbol (NotTerminate _) = True
isNotTerminateSymbol _ = False

nullables'' :: NullableSet -> NullableSet -> CFG -> NullableSet
nullables'' nullableSet nullableSet' cfg
  | nullableSet == nullableSet' = nullableSet
  | otherwise = nullables'' (nullables' nullableSet cfg) nullableSet cfg

type NullableSet = Set.Set NotTerminateSymbol

type NotTerminateSymbol = String
type TerminateSymbol = String

type CFGItem = (NotTerminateSymbol, [Symbol])
type CFG = [CFGItem]

data Symbol = NotTerminate NotTerminateSymbol
            | Terminate TerminateSymbol deriving (Show)

cfg1 :: CFG
cfg1 = [ ("Z", [Terminate "d"])
       , ("Z", [NotTerminate "X", NotTerminate "Y", NotTerminate "Z"])
       , ("Y", [Terminate "c"])
       , ("Y", [Terminate ""])
       , ("X", [NotTerminate "Y"])
       , ("X", [Terminate "a"]) ]
