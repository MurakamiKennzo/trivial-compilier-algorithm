-- define FA, DFA, NFA
-- FA accept states initState endStates transfer Function

data FA t a s = FA { literal :: a
                   , states :: [s]
                   , initState :: s
                   , endStates :: [s] 
                   , trans :: [t] }

type DFA a s = FA (s -> a -> s) a s

type NFA a s = FA (s -> a -> [s]) a s

dfa :: a -> [s] -> s -> [s] -> [s -> a -> s] -> DFA a s
dfa = FA

nfa :: a -> [s] -> s -> [s] -> [s -> a -> [s]] -> NFA a s
nfa = FA
