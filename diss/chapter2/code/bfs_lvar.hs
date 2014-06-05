-- l_acc is an LVar "output parameter":
bf_traverse :: ISet NodeLabel -> Graph ->
               NodeLabel -> Par ()
bf_traverse l_acc g startV =
  do putInSet l_acc startV 
     loop S.empty $singleton$startV
 where loop seen nu = 
         if nu == S.empty
         then return ()
         else do
           let seen' = union seen nu
           allNbr  <- parMap (nbrs g) nu
           allNbr' <- parFold union allNbr
           let nu' = difference allNbr' seen'
           -- Add to the accumulator:
           parMapM (putInSet l_acc) nu' 
           loop seen' nu'
-- The function `analyze' is applied to everything 
-- that is added to the set `analyzeSet':
go = do analyzedSet <- newSetWith analyze
        res <- bf_traverse analyzedSet profiles profile0
