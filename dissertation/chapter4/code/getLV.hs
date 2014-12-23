getLV :: (LVar a d) -> (a -> Bool -> IO (Maybe b)) 
                    -> (d -> IO (Maybe b)) -> Par e s b
getLV (LVar {state, status}) gThresh dThresh = 
 mkPar $ \k q -> do
  curStatus <- readIORef status
  case curStatus of
    Active listeners -> do
      tripped <- gThresh state False
      case tripped of
        Just b -> exec (k b) q  -- fast path
        Nothing -> do
          ifWinRace <- newDedupCheck (Sched.idemp q)
          let onUpdate d = unblockWhen (dThresh d)
              onFreeze   = unblockWhen (gThresh state True)
              unblockWhen thresh tok q = do
                tripped <- thresh
                whenJust tripped $ \b -> do        
                  B.remove tok
                  ifWinRace (Sched.pushWork q (k b)) (return ())
          tok <- B.put listeners (Listener onUpdate onFreeze)
          frozen   <- isFrozen status
          tripped' <- gThresh state frozen
          case tripped' of
            Just b -> do
              B.remove tok  -- remove the listener we just added, and
              ifWinRace (exec (k b) q) (sched q)
                -- execute our continuation, or go back to the scheduler.
            Nothing -> sched q
    Frozen -> do
      tripped <- gThresh state True
      case tripped of
        Just b -> exec (k b) q -- already past the threshold; invoke the
                               -- continuation immediately                    
        Nothing -> sched q     -- We'll NEVER be above the threshold.
