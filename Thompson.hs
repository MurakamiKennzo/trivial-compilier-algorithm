--  {-# LANGUAGE OverloadedStrings #-}

-- implements Thompson Algorithm that transform Regex to NFA

 module Thompson
  (
    thompson
  ) where

-- import Data.Aeson
-- import Data.Text ( Text )
-- import Data.Aeson.Text

thompson :: Regex a -> NFA a Int
thompson = fst . thompson' 0

thompson' :: (Enum s) => s -> Regex a -> (NFA a s, s)
thompson' s Empty = (return trans, s')
  where trans = NFATrans (InitState s) Nothing (EndState $ succ s)
        s' = succ . succ $ s

thompson' s (Literal a) = (return trans, s')
  where trans = NFATrans (InitState s) (return a) (EndState $ succ s)
        s' = succ . succ $ s

thompson' s (r :|: r') = (transes' <> transes'', newS)
  where (nfa', s') = thompson' s r
        (nfa'', s'') = thompson' s' r'
        newInitState = InitState s''
        newEndState = EndState $ succ s''
        newS = succ . succ $ s''
        transes' = choice nfa' (newInitState, newEndState)
        transes'' = choice nfa'' (newInitState, newEndState)

thompson' s (r :*: r') = (a <> [b'] <> bs <> [e] <> c <> [d'] <> ds, s'')
  where (nfa', s') = thompson' s r
        (nfa'', s'') = thompson' s' r'
        (a, b:bs) = break isEndState nfa'
        (c, d:ds) = break isInitState nfa''
        b' = endToMidState b
        d' = initToMidState d
        e = NFATrans (endState b') Nothing (startState d')

thompson' s (Closure r) = ([newInitToNewEndTrans, initTrans] <> a <> [b'] <> (if isSameTrans then [] else [d']) <> [endToInitTrans] <> bs <> [endTrans], newS)
  where (nfa', s') = thompson' s r
        newInitState = InitState s'
        newEndState = EndState $ succ s'
        newS = succ . succ $ s'
        (a, b:bs) = break isInitState nfa'
        (c, d:ds) = break isEndState nfa'
        isSameTrans = isEndState b
        b' = if isSameTrans then (endToMidState . initToMidState $ b) else initToMidState b
        d' = if isSameTrans then (initToMidState . endToMidState $ d) else endToMidState d
        initTrans = NFATrans newInitState Nothing (startState b')
        endTrans = NFATrans (endState d') Nothing newEndState
        newInitToNewEndTrans = NFATrans newInitState Nothing newEndState
        endToInitTrans = NFATrans (endState d') Nothing (startState b')

choice :: NFA a s -> (State s, State s) -> NFA a s
choice nfa (s, e) = let (a, b:bs) = break isInitState nfa
                        (c, d:ds) = break isEndState nfa
                        isSameTrans = isEndState b
                        b' = if isSameTrans then (endToMidState . initToMidState $ b) else initToMidState b
                        d' = if isSameTrans then (initToMidState . endToMidState $ d) else endToMidState d
                        initTrans = NFATrans s Nothing (startState b')
                        endTrans = NFATrans (endState d') Nothing e
                    in  a <> [initTrans] <> [b'] <> (if isSameTrans then [] else [d']) <> [endTrans] <> bs

isInitState :: NFATrans a s -> Bool
isInitState (NFATrans (InitState _) _ _) = True
isInitState _ = False

isEndState :: NFATrans a s -> Bool
isEndState (NFATrans _ _ (EndState _)) = True
isEndState _ = False

initToMidState :: NFATrans a s -> NFATrans a s
initToMidState (NFATrans (InitState s) a s') = NFATrans (MidState s) a s'
initToMidState a = a

endToMidState :: NFATrans a s -> NFATrans a s
endToMidState (NFATrans s' a (EndState s)) = NFATrans s' a (MidState s)
endToMidState a = a

data Regex a = Empty
             | Literal a
             | Regex a :|: Regex a
             | Regex a :*: Regex a
             | Closure (Regex a)

type NFA a s = [NFATrans a s]

data NFATrans a s = NFATrans { startState :: State s
                             , transItem :: Maybe a
                             , endState :: State s } deriving (Show, Eq)

data State s = EndState s
             | InitState s
             | MidState s deriving (Show, Eq)

-- toJSON test with Echarts
-- instance (ToJSON s) => ToJSON (State s) where
--   toJSON (EndState s) = 
--         object ["type" .= (2 :: Int), "value" .= s]
--   toJSON (InitState s) = 
--         object ["type" .= (0 :: Int), "value" .= s]
--   toJSON (MidState s) = 
--         object ["type" .= (1 :: Int), "value" .= s]

-- instance (ToJSON a, ToJSON s) => ToJSON (NFATrans a s) where
--   toJSON (NFATrans s a s') =
--         object ["startState" .= s, "transItem" .= a, "endState" .= s']
