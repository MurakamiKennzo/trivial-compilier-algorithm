{-# LANGUAGE ScopedTypeVariables #-}

module SubsetConstruction
  (
    subsetConstruction
  ) where

import qualified Data.Set as Set
import qualified Thompson as T
import Data.List ( (\\) )
import Data.Maybe ( isNothing
                  , isJust
                  , fromJust )
import Regex
import Data.Aeson.Text

subsetConstruction :: (Ord s, Ord a) => T.NFA a s -> Maybe (DFA a s)
subsetConstruction = toDFA . subsetConstruction''

toDFA :: (Ord s, Eq a) => T.NFATrans a (State s) -> Maybe (DFA a s)
toDFA nfaTrans
  | null startTrans = Nothing
  | otherwise = return $ DFA (T.StartState . Set.map extractTState . extractTState . T.startState . head $ startTrans) (map (\(T.NFATransItem _ a s'') -> (fromJust a, toDFA' s'' newNfaTrans)) startTrans)
  where startTrans = filter (isStartState . extractTState . T.startState) nfaTrans
        newNfaTrans = nfaTrans \\ startTrans

toDFA' :: (Ord s, Eq a) => T.State (State s) -> T.NFATrans a (State s) -> DFA a s
toDFA' s nfaTrans
  | isStartState s' = DFA (T.StartState s'') (map (\(T.NFATransItem _ a s'') -> (fromJust a, toDFA' s'' newNfaTrans)) trans)
  | isEndState s' = DFA (T.EndState s'') (map (\(T.NFATransItem _ a s'') -> (fromJust a, toDFA' s'' newNfaTrans)) trans)
  | otherwise = DFA (T.MidState s'') (map (\(T.NFATransItem _ a s'') -> (fromJust a, toDFA' s'' newNfaTrans)) trans)
  where trans = filter ((== s) . T.startState) nfaTrans
        s' = extractTState s
        s'' = Set.map extractTState s'
        newNfaTrans = nfaTrans \\ trans

extractTState :: T.State s -> s
extractTState (T.StartState s) = s
extractTState (T.MidState s) = s
extractTState (T.EndState s) = s

isStartState :: State s -> Bool
isStartState = any isTStartState

isTStartState :: T.State s -> Bool
isTStartState (T.StartState _) = True
isTStartState _ = False

isEndState :: State s -> Bool
isEndState = any isTEndState

isTEndState :: T.State s -> Bool
isTEndState (T.EndState _) = True
isTEndState _ = False

subsetConstruction'' :: (Ord s, Ord a) => T.NFA a s -> T.NFATrans a (State s)
subsetConstruction'' nfa@(T.StartNode s transformes) = subsetConstruction' workList states nfaTrans []
  where nfaTrans = T.toNFATrans nfa
        state = epsilonClosure (T.StartState s) mempty nfaTrans
        states = Set.insert state mempty
        workList = [state]

subsetConstruction' :: forall s a. (Ord s, Ord a) => WorkList s -> States s -> T.NFATrans a s -> T.NFATrans a (State s) -> T.NFATrans a (State s)
subsetConstruction' [] _ _ nfaTrans' = nfaTrans'
subsetConstruction' (state:workList) states nfaTrans nfaTrans' = subsetConstruction' newWorkList newStates nfaTrans nfaTrans''
  where transItems = findTransItems state nfaTrans
        (newWorkList, newStates, nfaTrans'') = Set.foldr subsetConstruction''' (workList, states, nfaTrans') transItems

        subsetConstruction''' :: (Ord s, Ord a) => Maybe a -> (WorkList s, States s, T.NFATrans a (State s)) -> (WorkList s, States s, T.NFATrans a (State s))
        subsetConstruction''' transformItem (workList, states, nfaTrans') = (newWorkList, newStates, nfaTrans'')
          where transStates = map T.endState . filter ((== transformItem) . T.transformItem) $ nfaTrans
                newState = foldMap (\s -> epsilonClosure s mempty nfaTrans) transStates
                isInStates = newState `Set.member` states
                newWorkList = if isInStates then workList else workList <> [newState]
                newStates = if isInStates then states else Set.insert newState states
                nfaTrans'' = nfaTrans' <> Set.toList (Set.singleton $ T.NFATransItem (T.MidState state) transformItem (T.MidState newState))

findTransItems :: (Ord s, Ord a) => State s -> T.NFATrans a s -> Set.Set (Maybe a)
findTransItems s nfaTrans = foldr Set.insert mempty . map T.transformItem $ nfaTransItems
  where nfaTransItems = filter (\(T.NFATransItem s' a s'') -> s' `Set.member` s && isJust a) nfaTrans

epsilonClosure :: (Ord s) => T.State s -> State s -> T.NFATrans a s -> State s
epsilonClosure s visitedStates nfaTrans = Set.insert s nFATransAllStates
  where nfaTransItems = filter (\(T.NFATransItem s' a s'') -> s == s' && isNothing a) nfaTrans
        nFATransStates = map (\(T.NFATransItem s' a s'') -> s'') nfaTransItems
        visitedStates' = Set.insert s visitedStates
        nFATransStates' = filter (not . (`Set.member` visitedStates)) nFATransStates
        nFATransAllStates = foldMap (\s' -> epsilonClosure s' visitedStates' nfaTrans) nFATransStates'

type State s = Set.Set (T.State s)

type States s = Set.Set (State s)

type WorkList s = [State s]

data DFA a s = DFA (T.State (Set.Set s)) [(a, DFA a s)]
            deriving (Show)

-- below for echarts test
toNFATrans :: Maybe (DFA a s) -> T.NFATrans a (Set.Set s)
toNFATrans Nothing = []
toNFATrans (Just (DFA s transformes)) = map (toNFATransItem s) transformes <> foldMap (toNFATrans . return . snd) transformes

toNFATransItem :: T.State (Set.Set s) -> (a, DFA a s) -> T.NFATransItem a (Set.Set s)
toNFATransItem s (a, DFA s' _) = T.NFATransItem s (return a) s'
