module Hopcroft
  (
    hopcroft
  ) where

import qualified Data.Set as Set
import qualified Thompson as T
import qualified SubsetConstruction as SC
import Regex
import Data.Aeson.Text
import Debug.Trace
import Data.Maybe ( fromJust )
import Data.Function ( on )
import Data.List ( partition
                 , find
                 , sort
                 , groupBy )

hopcroft :: (Ord s, Ord a, Show s) => Maybe (SC.DFA a s) -> Maybe (MinDFA a s)
hopcroft dfa = minDFA
  where nfaTrans = SC.toNFATrans dfa
        states = allStates nfaTrans
        (a, n) = Set.partition SC.isTEndState states
        states' = splitState n nfaTrans `Set.union` splitState a nfaTrans
        states'' = mergeState states'
        mStartState = find SC.isTStartState states''
        minDFA = mStartState >>= \startState -> snd $ buildMinDFA startState mempty states'' nfaTrans

buildMinDFA :: (Ord s, Ord a, Show s) => State s -> Set.Set (State s) -> Set.Set (State s) -> T.NFATrans a (Set.Set s) -> (Set.Set (State s), Maybe (MinDFA a s))
buildMinDFA state visitedStates states nfaTrans
  | state `Set.member` visitedStates = (visitedStates, Nothing)
  | otherwise = (visitedStates', return $ MinDFA state newnfaTranformes') 
  where state' = SC.extractTState state
        nfaTrans' = filter ((`Set.isSubsetOf` state') . SC.extractTState . T.startState) nfaTrans
        newnfaTrans = map (\(T.NFATransItem s a s') -> T.NFATransItem state a (getStateConstructor s' . fromJust $ find ((SC.extractTState s') `Set.isSubsetOf`) (Set.map SC.extractTState states))) nfaTrans'
        newnfaTranformes = map groupTranItems . groupBy ((==) `on` T.endState) . sort $ newnfaTrans
        (visitedStates', newnfaTranformes') = foldr 
          ((\(items, state') (visitedStates, transformes) -> let (visitedStates', mMinDFA) = buildMinDFA state' visitedStates states nfaTrans 
                                                             in  (visitedStates', (items, maybe (MinDFA state' []) id mMinDFA):transformes))) 
          (Set.insert state visitedStates, []) newnfaTranformes

getStateConstructor :: T.State s -> a -> T.State a
getStateConstructor (T.StartState _) = T.StartState
getStateConstructor (T.EndState _) = T.EndState
getStateConstructor (T.MidState _) = T.MidState

groupTranItems :: (Ord a) => [T.NFATransItem a s] -> (Set.Set a, T.State s)
groupTranItems trans = let items = map fromJust . map T.transformItem $ trans
                           state = T.endState . head $ trans
                       in  (Set.fromList items, state)

mergeState :: (Ord s) => Set.Set (Set.Set (State s)) -> Set.Set (State s)
mergeState states = Set.map (\s -> findConstructor s $ foldMap SC.extractTState s) states
  where findConstructor :: Set.Set (State s) -> a -> T.State a
        findConstructor state
          | any SC.isTStartState state = T.StartState
          | any SC.isTEndState state = T.EndState
          | otherwise = T.MidState

splitState :: (Ord s) => Set.Set (State s) -> T.NFATrans a (Set.Set s) -> Set.Set (Set.Set (State s))
splitState states nfaTrans
  | Set.null outStates = Set.insert inStates mempty
  | Set.null inStates = Set.insert outStates mempty
  | otherwise = splitState inStates nfaTrans `Set.union` splitState outStates nfaTrans
  where lensnfaTrans = filter ((`Set.member` states) . T.startState) nfaTrans
        (inLensnfaTrans, outLensnfaTrans) = partition ((`Set.member` states) . T.endState) lensnfaTrans
        inStates = Set.fromList $ map T.startState inLensnfaTrans
        outStates = Set.fromList $ map T.startState outLensnfaTrans

allStates :: (Ord s) => T.NFATrans a (Set.Set s) -> Set.Set (State s)
allStates [] = mempty
allStates (T.NFATransItem s _ s': xs) = Set.insert s . Set.insert s' $ allStates xs

type State s = T.State (Set.Set s)

data MinDFA a s = MinDFA (State s) [(Set.Set a, MinDFA a s)]
                    deriving (Show)

-- below for echarts test
toNFATrans :: Maybe (MinDFA a s) -> T.NFATrans (Set.Set a) (Set.Set s)
toNFATrans Nothing = []
toNFATrans (Just (MinDFA s transformes)) = map (toNFATransItem s) transformes <> foldMap (toNFATrans . return . snd) transformes

toNFATransItem :: T.State (Set.Set s) -> ((Set.Set a), MinDFA a s) -> T.NFATransItem (Set.Set a) (Set.Set s)
toNFATransItem s (a, MinDFA s' _) = T.NFATransItem s (return a) s'
