getLV :: (LVar a d) -> (a -> Bool -> IO (Maybe b)) 
                    -> (d -> IO (Maybe b)) -> Par lvl b
getLV (LVar{state, status}) gThresh dThresh = 
  mkPar $\k q -> 
    let onUpd d = unblockWhen (dThresh d)
        onFrz   = unblockWhen (gThresh state True)
        unblockWhen thresh tok q = do
          tripped <- thresh
          whenJust tripped $ \b -> do
            B.remove tok
            Sched.pushWork q (k b)                     
    in do
      curStat <- readIORef status
      case curStat of
        Frozen -> do -- no further deltas can arrive!
          tripped <- gThresh state True
          case tripped of
            Just b  -> exec (k b) q
            Nothing -> sched q     
        Active ls -> do
          tok <- B.put ls (Listener onUpd onFrz)
          frz <- isFrozen status -- must recheck after enrolling listener
          tripped <- gThresh state frz
          case tripped of
            Just b  -> do
              B.remove tok -- remove the listener 
              k b q        -- execute our continuation
            Nothing -> sched q
