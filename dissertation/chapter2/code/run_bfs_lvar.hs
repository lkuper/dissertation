{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- Harness to run the actual code used in the paper:

-- import LVarTracePure
import Data.Set as S

data NodeLabel = NodeLabel Int
  deriving (Show,Eq,Ord)
data Graph

-- Hack to make sets print nicely and also run:
leftBrace x _ = S.singleton x
rightBrace = error "rightBrace: this is a hack.  Shouldn't be touched."

-- Temporary: STUBS to make things typecheck:
nbrs = error "Neighbors unimplemented..."

p0 :: NodeLabel 
p0 = undefined
p0Sing = S.singleton p0

analyze :: NodeLabel -> Double
analyze (NodeLabel i) = fromIntegral i + 3.3

profiles :: Graph
profiles = undefined

-- HACK: abstract parMap over sets and lists:
-- class Col c where
--   toList :: c -> [NodeLabel]
-- instance Col [NodeLabel] where
-- instance Col (Set NodeLabel) where  

parMap :: (NodeLabel -> b) -> Set NodeLabel -> Par (Set b)
-- parMap :: (NodeLabel -> b) -> [NodeLabel] -> Set b
-- parMap :: (Col c1, Col c2) => (NodeLabel -> b) -> c1 -> c2
-- parMap :: (Col c1, Col c2) => (NodeLabel -> b) -> c1 -> Set NodeLabel
parMap = undefined


parMapM :: (NodeLabel -> Par b) -> Set NodeLabel -> Par (Set b)
parMapM = undefined

parMapS :: (NodeLabel -> b) -> Set NodeLabel -> Set b
parMapS = undefined

parFold :: (a -> a -> a) -> Set a -> Par a 
parFold = undefined

-- A set with an input and output end, of different types:
data IOSet a b = IOSet (ISet a) (ISet b)

-- A set with a builtin function.  We can implement this with callbacks:
newSetWith :: (a -> b) -> Par (ISet a)
newSetWith = undefined

--putInSet :: NodeLabel -> ISet NodeLabel -> Par ()
putInSet :: ISet NodeLabel -> NodeLabel -> Par () 
putInSet = undefined

type Par a = IO a 
type ISet a = Set a



#include "bfs_lvar.hs"
        return ()

main = putStrLn "Yay!  LVar ver got through typecheck at least."
