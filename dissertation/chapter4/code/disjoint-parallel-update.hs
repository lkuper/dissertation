{-# LANGUAGE TypeFamilies #-}

import Prelude hiding (read)
import Control.LVish
import Control.Par.ST (liftST)
import Control.Par.ST.Vec (ParVecT, set, reify, forkSTSplit, write, read, runParVecT)
import Data.Vector (freeze, toList)

p :: (HasGet e, HasPut e) => ParVecT s1 String Par e s [String]
p = do
  -- Fill all six slots in the vector with "foo".
  set "foo"
  -- Get a pointer to the state.
  ptr <- reify

  -- Fork two computations, each of which has access to half the
  -- vector.  Within the two forked child computations, `ptr` is
  -- inaccessible.
  forkSTSplit 3 -- Split at index 3 in the vector.
              (write 0 "bar")
              (write 0 "baz")

  frozen <- liftST (freeze ptr)
  return (toList frozen)

main = print (runPar (runParVecT 6 p))
