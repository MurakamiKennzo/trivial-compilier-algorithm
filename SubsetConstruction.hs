import qualified Data.Set as Set
import qualified Thompson as T
import Data.Maybe ( isNothing
                  , isJust )
import Regex

subsetConstruction'' :: (Show s, Ord s, Ord a) => T.NFA a s -> T.NFATrans a (State s)
subsetConstruction'' nfa@(T.StartNode s transformes) = subsetConstruction' workList states nfaTrans []
  where nfaTrans = T.toNFATrans nfa
        state = epsilonClosure (T.StartState s) mempty nfaTrans
        states = Set.insert state mempty
        workList = [state]

subsetConstruction' :: (Show s, Ord s, Ord a) => WorkList s -> States s -> T.NFATrans a s -> T.NFATrans a (State s) -> T.NFATrans a (State s)
subsetConstruction' [] _ _ nfaTrans' = nfaTrans'
subsetConstruction' (state:workList) states nfaTrans nfaTrans' = subsetConstruction' newWorkList newStates nfaTrans nfaTrans''
  where (transItems, transStates) = findTranses state nfaTrans
        newState = foldMap (\s -> epsilonClosure s mempty nfaTrans) transStates
        isInStates = newState `Set.member` states
        newWorkList = if isInStates then workList else workList <> [newState]
        newStates = if isInStates then states else Set.insert newState states
        nfaTrans'' = nfaTrans' <> Set.toList (Set.map (\transItem -> T.NFATransItem (T.MidState state) transItem (T.MidState newState)) transItems)

findTranses :: (Ord s, Ord a) => State s -> T.NFATrans a s -> (Set.Set (Maybe a), State s)
findTranses s nfaTrans = buildTranses nFATransItems (mempty, mempty)
  where nFATransItems = filter (\(T.NFATransItem s' a s'') -> s' `Set.member` s && isJust a) nfaTrans
        buildTranses :: (Ord s, Ord a) => T.NFATrans a s -> (Set.Set (Maybe a), State s) -> (Set.Set (Maybe a), State s)
        buildTranses [] a = a   
        buildTranses (T.NFATransItem _ a s:xs) (m, n) = buildTranses xs (Set.insert a m, Set.insert s n)

epsilonClosure :: (Ord s) => T.State s -> State s -> T.NFATrans a s -> State s
epsilonClosure s visitedStates nfaTrans = Set.insert s nFATransAllStates
  where nFATransItems = filter (\(T.NFATransItem s' a s'') -> s == s' && isNothing a) nfaTrans
        nFATransStates = map (\(T.NFATransItem s' a s'') -> s'') nFATransItems
        visitedStates' = Set.insert s visitedStates
        nFATransStates' = filter (not . (`Set.member` visitedStates)) nFATransStates
        nFATransAllStates = foldMap (\s' -> epsilonClosure s' visitedStates' nfaTrans) nFATransStates'

type State s = Set.Set (T.State s)

type States s = Set.Set (State s)

type WorkList s = [State s]

data DFA a s = StartNode (State s) [(a, DFA a s)]
             | MidNode (States s) [(a, DFA a s)]
             | EndNode (States s) deriving (Show)
