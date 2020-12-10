-- write a DFA analysis algorithm that can generate accept string.
-- for example:
-- here is the dfa:
-- dfa :: DFA
-- dfa = fromList 0 [2, 4] [ (0, 'i', 1)
--                         , (1, 'f', 2)
--                         , (2, 'i', 3)
--                         , (3, 'f', 4) ]

-- then if you run
-- nextToken "ifif" dfa
-- will get "ifi"

-- if you run
-- nextToken "ifii" dfa
-- will get "if"

module NextToken
  (
    nextToken
  ) where

import qualified Data.Map as Map
import Data.List ( nub )
import Debug.Trace

nextToken :: String -> DFA -> Maybe String
nextToken s dfa = do
  (init, _) <- Map.lookupMin dfa
  s' <- nextToken' "" (return init) [] s dfa
  return s'

nextToken' :: String -> Maybe State -> [State] -> String -> DFA -> Maybe String
nextToken' s' state stack s dfa
  | state == Nothing = rollString s' state stack
  | otherwise = nextState s' state stack s dfa

nextState :: String -> Maybe State -> [State] -> String -> DFA -> Maybe String
nextState s' state stack "" dfa = rollString s' state stack
nextState s' state stack (x:xs) dfa = do
  state' <- state
  let ss' = x:s'
      stack' = if isEndState state' then [state'] else state':stack
      state'' = dfa Map.!? state' >>= Map.lookup x
  ss'' <- nextToken' ss' state'' stack' xs dfa
  return $ ss''

  where isEndState :: State -> Bool
        isEndState (EndState _) = True
        isEndState _ = False

rollString :: String -> Maybe State -> [State] -> Maybe String
rollString "" _ _ = Nothing
rollString (x:xs) state stack
  | isAcceptState state = return $ reverse (x:xs)
  | otherwise = case stack of
                  (s:ss) -> rollString xs (return s) ss
                  _ -> Nothing
  where isAcceptState :: Maybe State -> Bool
        isAcceptState (Just (EndState _)) = True
        isAcceptState _ = False

type DFA = Map.Map State (Map.Map Char State)

fromList :: Int -> [Int] -> [(Int, Char, Int)] -> DFA
fromList _ _ [] = mempty
fromList init ends ((s, c, e):trans) = Map.insertWith Map.union (consState s) (Map.singleton c (consState e)) $ fromList init ends trans
  where states = map consState . nub $ foldr (\(s, _, e) xs -> s:e:xs) [] trans
        chars = foldr (\(_, c, _) xs -> c:xs) [] trans

        consState :: Int -> State
        consState n
          | init == n = InitState n
          | n `elem` ends = EndState n
          | otherwise = MidState n
        
data State = InitState Int
           | MidState Int
           | EndState Int deriving (Show, Eq, Ord)
