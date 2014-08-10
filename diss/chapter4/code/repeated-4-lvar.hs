{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish  -- Generic scheduler; works with all LVars.
import Data.LVar.IVar -- The particular LVar we need for this program.

p :: (HasPut e, HasGet e) => Par e s Int
p = do
  num <- new
  fork (put num 4)
  fork (put num 4)
  get num

main = print $ runPar p
