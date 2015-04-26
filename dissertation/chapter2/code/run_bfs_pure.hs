{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- N.B.: This is just a harness to make sure that the code in
-- `bfs_pure.hs' typechecks.

import Data.Set

data NodeLabel = NodeLabel Int
  deriving (Show, Eq, Ord)
data Graph

-- Stubs to make things typecheck:

nbrs :: Graph -> NodeLabel -> Set NodeLabel
nbrs = undefined

profile0 :: NodeLabel 
profile0 = undefined

analyze :: NodeLabel -> Double
analyze (NodeLabel i) = fromIntegral i + 3.3

profiles :: Graph
profiles = undefined

parMap :: (NodeLabel -> b) -> Set NodeLabel -> Set b
parMap = undefined

parFold :: (a -> a -> a) -> Set a -> a 
parFold = undefined

#include "bfs_pure.hs"

main = putStrLn "Yay!  Got through typecheck at least."
