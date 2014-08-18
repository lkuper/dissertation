{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-- Harness to run the actual code used in the paper:

--import LVarTracePure
import Data.Set as S

data NodeLabel = NodeLabel Int
  deriving (Show,Eq,Ord)
data Graph

-- Hack to make sets print nicely and also run:
leftBrace x _ = S.singleton x
rightBrace = error "rightBrace: this is a hack.  Shouldn't be touched."

-- Temporary: STUBS to make things typecheck:
nbrs = error "Neighbors unimplemented..."

profile0 :: NodeLabel 
profile0 = undefined
profile0Sing = S.singleton profile0

analyze :: NodeLabel -> Double
analyze (NodeLabel i) = fromIntegral i + 3.3

profiles :: Graph
profiles = undefined

-- HACK: abstract parMap over sets and lists:
-- class Col c where
--   toList :: c -> [NodeLabel]
-- instance Col [NodeLabel] where
-- instance Col (Set NodeLabel) where  

parMap :: (NodeLabel -> b) -> Set NodeLabel -> Set b
-- parMap :: (NodeLabel -> b) -> [NodeLabel] -> Set b
-- parMap :: (Col c1, Col c2) => (NodeLabel -> b) -> c1 -> c2
-- parMap :: (Col c1, Col c2) => (NodeLabel -> b) -> c1 -> Set NodeLabel
parMap = undefined

parMapS :: (NodeLabel -> b) -> Set NodeLabel -> Set b
parMapS = undefined

parFold :: (a -> a -> a) -> Set a -> a 
parFold = undefined



#include "bfs_pure.hs"
-- ================================================================================

-- #include "bfs_lvar.hs"

-- TODO: Finish me.


main = putStrLn "Yay!  Got through typecheck at least."
