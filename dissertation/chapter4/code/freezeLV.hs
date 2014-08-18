freezeLV :: LVar a d -> Par QuasiDet ()
freezeLV (LVar {status}) = mkPar $ \k q -> do
  Sched.awaitClear q
  oldStat <- atomicModifyIORef status $ \s->(Frozen, s)    
  case oldStat of
    Frozen -> return ()
    Active listeners -> B.foreach listeners $ 
      \(Listener _ onFrz) tok -> onFrz tok q
  k () q
