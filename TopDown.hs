-- Give a CFG:
-- S -> N V N
-- N -> s | t | g | w
-- V -> e | d
-- write a function that accept a sentence and answer the CFG can or not deduce the sentence, using top-down analysis.

module TopDown
  (
    contains
  ) where

import qualified Data.Map as Map
import Control.Monad ( guard )

contains :: Sentence -> CFG -> Bool
contains sentence cfg = contains' [] (startSymbol cfg) cfg cfg sentence

contains' :: SymbolsStack -> NotTerminateSymbol -> CFG -> CFG -> Sentence -> Bool
contains' stack notTerminateSymbol cfg originCfg sentence = maybe (rollback (if null stack then [] else tail stack) cfg originCfg sentence) id $ do
  let productionMap = production cfg
  symbolsList <- productionMap Map.!? notTerminateSymbol
  guard (length symbolsList /= 0)
  let (currentSymbols:restSymbolsList) = symbolsList
      cfg' = cfg { production = Map.adjust tail notTerminateSymbol productionMap }
  return $ accordCurrentSymbols currentSymbols stack cfg' originCfg sentence

accordCurrentSymbols :: Symbols -> SymbolsStack -> CFG -> CFG -> Sentence -> Bool
accordCurrentSymbols symbols stack cfg originCfg [] = rollback stack cfg cfg []
accordCurrentSymbols symbols stack cfg originCfg sentence@(x:xs) = 
  case headSymbol of
    (Terminate (TerminateSymbol string)) -> accordEqualSentencePart string 
    _ -> let stack'@(symbolsStackItem:_) = changeSymbolsStack symbols stack sentence
             mNotTerminateSymbol = findNotTerminateSymbol . fst $ symbolsStackItem
         in  case mNotTerminateSymbol of
               (Just (NotTerminate notTerminateSymbol)) -> contains' stack' notTerminateSymbol cfg originCfg sentence
               Nothing -> if null xs then True else rollback stack cfg originCfg sentence
  where headSymbol = getHeadSymbol symbols
        accordEqualSentencePart :: String -> Bool
        accordEqualSentencePart x'
          | x' == x = let stack'@(symbolsStackItem:_) = changeSymbolsStack symbols stack xs
                          mNotTerminateSymbol = findNotTerminateSymbol . fst $ symbolsStackItem
                      in  case mNotTerminateSymbol of
                            (Just (NotTerminate notTerminateSymbol)) -> contains' stack' notTerminateSymbol (cfg { production = Map.insert notTerminateSymbol (maybe [] id $ (production originCfg) Map.!? notTerminateSymbol) (production cfg) }) originCfg xs
                            Nothing -> if null xs then True else rollback stack cfg originCfg sentence
          | otherwise = rollback stack cfg originCfg sentence

changeSymbolsStack :: Symbols -> SymbolsStack -> Sentence -> SymbolsStack
changeSymbolsStack symbols [] sentence = (symbols, sentence): []
changeSymbolsStack symbols (x:xs) sentence = case fst x of
  (NotTerminate _ :-: symbolsList) -> (symbols :-: symbolsList, sentence):x:xs
  (symbols'@(Terminate _) :-: symbolsList) -> let ((symbols'', sentence'):restStack') = changeSymbolsStack symbols ((symbolsList, snd x): xs) sentence
                                              in  (symbols' :-: symbols'', sentence'):x:restStack'
  _ -> (symbols, sentence):x: xs

getHeadSymbol :: Symbols -> Symbols
getHeadSymbol (x :-: _) = x
getHeadSymbol x = x

findNotTerminateSymbol :: Symbols -> Maybe Symbols
findNotTerminateSymbol symbols@(NotTerminate _) = return symbols
findNotTerminateSymbol symbols@(Terminate _) = Nothing
findNotTerminateSymbol (symbols@(NotTerminate _):-: xs) = return symbols
findNotTerminateSymbol (_:-: xs) = findNotTerminateSymbol xs

rollback :: SymbolsStack -> CFG -> CFG -> Sentence -> Bool
rollback [] _ _ _ = False
rollback stack@((symbols, sentence):restSymbolsStack) cfg originCfg sentence' = let mNotTerminateSymbol = findNotTerminateSymbol symbols
                                                                                in  case mNotTerminateSymbol of
                                                                                      (Just (NotTerminate notTerminateSymbol)) -> contains' stack notTerminateSymbol cfg originCfg sentence
                                                                                      Nothing -> rollback restSymbolsStack cfg originCfg sentence'

type SymbolsStack = [(Symbols, Sentence)]

type Sentence = [String]

newtype NotTerminateSymbol = NotTerminateSymbol String deriving (Show, Eq, Ord)
newtype TerminateSymbol = TerminateSymbol String deriving (Show)

data CFG = CFG { startSymbol :: NotTerminateSymbol
               , production :: Map.Map NotTerminateSymbol [ Symbols ] } deriving (Show)

infixr 5 :-:
data Symbols = NotTerminate NotTerminateSymbol
             | Terminate TerminateSymbol
             | Symbols :-: Symbols deriving (Show)

cfg :: CFG
cfg = CFG { startSymbol = NotTerminateSymbol "S"
          , production = Map.fromList [ (NotTerminateSymbol "S", [ NotTerminate (NotTerminateSymbol "N") :-: NotTerminate (NotTerminateSymbol "V") :-: NotTerminate (NotTerminateSymbol "N") ])
                                      , (NotTerminateSymbol "N", [ Terminate (TerminateSymbol "s")
                                                                 , Terminate (TerminateSymbol "t")
                                                                 , Terminate (TerminateSymbol "g")
                                                                 , Terminate (TerminateSymbol "w") ])
                                      , (NotTerminateSymbol "V", [ Terminate (TerminateSymbol "e")
                                                                 , Terminate (TerminateSymbol "d") ]) ] }
