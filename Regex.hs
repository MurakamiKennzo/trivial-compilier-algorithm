-- Define a regex data type

module Regex
  (
    Regex(..)
  ) where

data Regex a = Empty
             | Literal a
             | Regex a :|: Regex a
             | Regex a :*: Regex a
             | Closure a
