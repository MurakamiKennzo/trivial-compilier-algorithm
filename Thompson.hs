 {-# LANGUAGE OverloadedStrings #-}

-- implements Thompson Algorithm that transform Regex to NFA

 module Thompson
  (
    thompson
  ) where

import Regex
import Control.Arrow ( second )
import Data.Aeson
import Data.Aeson.Text

thompson :: Regex a -> NFA a Int
thompson = snd . thompson' 0

thompson' :: Int -> Regex a -> (Int, NFA a Int)
thompson' n Empty = (n + 2, StartNode n [ (Nothing, EndNode $ n + 1) ])
thompson' n (Literal a) = (n + 2, StartNode n [ (Just a, EndNode $ n + 1) ])
thompson' n (regex :|: regex') = (n'' + 2, result)
  where (n', nfa') = thompson' n regex
        (n'', nfa'') = thompson' n' regex'
        newStartState = n''
        newEndState = n'' + 1
        result = addNewEndState newEndState . addNewStartState newStartState $ (nfa', nfa'')
thompson' n (regex :*: regex') = (n'', result)
  where (n', nfa') = thompson' n regex
        (n'', nfa'') = thompson' n' regex'
        result = connect nfa' nfa''
thompson' n (Closure regex) = (n' + 2, result)
  where (n', nfa') = thompson' n regex
        newStartState = n'
        newEndState = n' + 1
        result = insertClosure (newStartState, newEndState) nfa'

addNewStartState :: s -> (NFA a s, NFA a s) -> NFA a s
addNewStartState s (StartNode s' transformes', StartNode s'' transformes'') = 
  StartNode s [ (Nothing, MidNode s' transformes')
              , (Nothing, MidNode s'' transformes'') ]

addNewEndState :: s -> NFA a s -> NFA a s
addNewEndState s (EndNode s') = MidNode s' [ (Nothing, EndNode s) ]
addNewEndState s (StartNode s' transformes) = StartNode s' $ map (second $ addNewEndState s) transformes
addNewEndState s (MidNode s' transformes) = MidNode s' $ map (second $ addNewEndState s) transformes

connect :: NFA a s -> NFA a s -> NFA a s
connect (EndNode s) (StartNode s' transformes) = MidNode s [ (Nothing, MidNode s' transformes) ]
connect (StartNode s transformes) nfa = StartNode s $ map (second $ flip connect nfa) transformes
connect (MidNode s transformes) nfa = MidNode s $ map (second $ flip connect nfa) transformes

insertClosure :: (s, s) -> NFA a s -> NFA a s
insertClosure (newStartState, newEndState) (StartNode s transformes)
  = StartNode newStartState [ (Nothing, MidNode s $ map (second $ insertClosure' (newStartState, newEndState) s) transformes)
                            , (Nothing, EndNode newEndState) ]
insertClosure _ nfa = nfa

insertClosure' :: (s, s) -> s -> NFA a s -> NFA a s
insertClosure' (newStartState, newEndState) startState (EndNode s)
  = MidNode s [ (Nothing, EndNode newEndState)
              , (Nothing, MidNode startState []) ]
insertClosure' a b (StartNode c transformes) = StartNode c $ map (second $ insertClosure' a b) transformes
insertClosure' a b (MidNode c transformes) = MidNode c $ map (second $ insertClosure' a b) transformes

data NFA a s = StartNode s [(Maybe a, NFA a s)]
             | MidNode s [(Maybe a, NFA a s)]
             | EndNode s deriving (Show, Eq)

-- below for echarts test
toNFATrans :: NFA a s -> NFATrans a s
toNFATrans (StartNode s transformes) = map (toNFATransItem (StartState s)) transformes <> foldMap (toNFATrans . snd) transformes
toNFATrans (MidNode s transformes) = map (toNFATransItem (MidState s)) transformes <> foldMap (toNFATrans . snd) transformes
toNFATrans (EndNode s) = []

toNFATransItem :: State s -> (Maybe a, NFA a s) -> NFATransItem a s
toNFATransItem state (a , StartNode s _) = NFATransItem state a (StartState s)
toNFATransItem state (a , MidNode s _) = NFATransItem state a (MidState s)
toNFATransItem state (a , EndNode s) = NFATransItem state a (EndState s)

type NFATrans a s = [ NFATransItem a s ]

data NFATransItem a s = NFATransItem { startState :: State s
                                     , transformItem :: Maybe a
                                     , endState :: State s } deriving (Show)

data State s = EndState s
             | StartState s
             | MidState s deriving (Show)

instance (ToJSON s) => ToJSON (State s) where
  toJSON (EndState s) = 
        object ["type" .= (2 :: Int), "value" .= s]
  toJSON (StartState s) = 
        object ["type" .= (0 :: Int), "value" .= s]
  toJSON (MidState s) = 
        object ["type" .= (1 :: Int), "value" .= s]

instance (ToJSON a, ToJSON s) => ToJSON (NFATransItem a s) where
  toJSON (NFATransItem s a s') =
        object ["startState" .= s, "transformItem" .= a, "endState" .= s']
