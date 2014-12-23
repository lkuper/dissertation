putLV :: LVar a d -> (a -> IO (Maybe d)) -> Par e s ()
putLV (LVar{state, status}) doPut = mkPar $ \k q -> do  
  Sched.mark q  -- publish our intent to modify the LVar
  delta   <- doPut state      -- possibly modify LVar
  curStat <- readIORef status -- read while q is marked
  Sched.clearMark q           -- retract our intent
  whenJust delta $ \d -> do
    case curStat of
      Frozen -> error "Attempt to change a frozen LVar"
      Active listeners -> B.foreach listeners $ 
        \(Listener onUpd _) tok -> onUpd d tok q
  k () q 
