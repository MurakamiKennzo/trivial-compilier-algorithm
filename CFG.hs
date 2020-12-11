-- define a CFG that:
-- t -> terminate symbol set
-- n -> not terminate symbol set
-- p -> product rule
-- s -> start symbol

module CFG
  (
    CFG
  ) where

data CFG a b = CFG { t :: [a]
                   , n :: [b]
                   , p :: [b -> [CFGp a b]]
                   , s :: b }

data CFGp a b = CFGpa a
              | CFGpb b
