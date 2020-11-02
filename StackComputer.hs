-- Give an integer greater than or equality to 0. and an add operator, 
-- and a stack computer just have two instruction -- push and add,
-- computer the expression like 1+2+3.

module StackComputer
  (
    sumExpr
  ) where

import Control.Monad

sumExpr :: String -> Maybe Int
sumExpr = fmap execute . fmap readLexTree . readExpr

execute :: [StackComputer] -> Int
execute = head . execute' []

execute' :: [Int] -> [StackComputer] -> [Int]
execute' xs [] = xs
execute' (y:x:xs) (Add:zs) = execute' ((x+y): xs) zs
execute' xs (Push n:zs) = execute' (n: xs) zs

readLexTree :: LexTree -> [StackComputer]
readLexTree (LexInt n) = [Push n]
readLexTree ((left, right) :-> _) = readLexTree left <> readLexTree right <> [Add]

readExpr :: String -> Maybe LexTree
readExpr s = join $ do
  (x, s') <- readLexInt s
  return $ readExpr' x s'

readExpr' :: LexTree -> String -> Maybe LexTree
readExpr' x "" = return x
readExpr' x s = join $ do
  (_, s') <- readAdd s
  (y, s'') <- readLexInt s'
  let a = (x, y) :-> LexAdd
  return $ readExpr' a s''

readAdd :: String -> Maybe ((), String)
readAdd ('+':xs) = return ((), xs)
readAdd _ = mzero

readLexInt :: String -> Maybe (LexTree, String)
readLexInt (x:xs)
  | x `elem` ['0' .. '9'] = readLexInt' [x] xs
  | otherwise = mzero
readLexInt _ = mzero

readLexInt' :: String -> String -> Maybe (LexTree, String)
readLexInt' xs a@(y:ys)
  | y `elem` ['0' .. '9'] = readLexInt' (y:xs) ys
  | otherwise = return (LexInt . read $ reverse xs, a)
readLexInt' xs "" = return (LexInt . read $ reverse xs, "")

data LexTree = LexInt Int
             | (LexTree, LexTree) :-> LexAdd deriving (Show)

data LexAdd = LexAdd deriving (Show)

data StackComputer = Push Int
                   | Add deriving (Show)
