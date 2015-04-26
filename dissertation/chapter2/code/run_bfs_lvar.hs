{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- N.B.: This is just a harness to make sure that the code in
-- `bfs_lvar.hs' typechecks.  Moreover, `bfs_lvar.hs' doesn't actually
-- use the LVish library (nor does it actually appear in the
-- document); it's just here as an example of the pipelined
-- programming style.

import Data.Set

data NodeLabel = NodeLabel Int
  deriving (Show, Eq, Ord)
data Graph

-- Stubs to make things typecheck:

nbrs = undefined

profile0 :: NodeLabel 
profile0 = undefined

analyze :: NodeLabel -> Double
analyze (NodeLabel i) = fromIntegral i + 3.3

profiles :: Graph
profiles = undefined

parMap :: (NodeLabel -> b) -> Set NodeLabel -> Par (Set b)
parMap = undefined

parMapM :: (NodeLabel -> Par b) -> Set NodeLabel -> Par (Set b)
parMapM = undefined

parMapS :: (NodeLabel -> b) -> Set NodeLabel -> Set b
parMapS = undefined

parFold :: (a -> a -> a) -> Set a -> Par a 
parFold = undefined

-- A set with a builtin function.  We can implement this with callbacks:
newSetWith :: (a -> b) -> Par (ISet a)
newSetWith = undefined

--putInSet :: NodeLabel -> ISet NodeLabel -> Par ()
putInSet :: ISet NodeLabel -> NodeLabel -> Par () 
putInSet = undefined

type Par a = IO a 
type ISet a = Set a

#include "bfs_lvar.hs"

main = putStrLn "Yay!  LVar ver got through typecheck at least."
