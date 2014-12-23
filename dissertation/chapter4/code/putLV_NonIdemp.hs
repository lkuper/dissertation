putLV :: LVar a d -> (a -> IO (Maybe d)) -> Par e s ()
putLV_ lv@(LVar {state, status, name, handlerStatus}) doPut = 
  mkPar $ \ k q -> 
   let cont (delta, ret) = ClosedPar $ \q -> do
         curStatus <- readIORef status -- read frozen bit while q's status is marked
         clearPutFlag q                -- retract our modification intent
         whenJust delta $ \d -> do
           case curStatus of
             Frozen -> throw (PutAfterFreezeExn "Attempt to change a frozen LVar")
             Active listeners -> do
               B.foreach listeners $ \(Listener onUpdate _) tok -> do
                 onUpdate d tok q
         exec (k ret) q 

       execPut = exec (close (doPut state) cont) q -- possibly modify LVar  

       putIdemp = do
         setPutFlag q -- publish our intent to modify the LVar
         execPut      -- do the modification (and subsequently clear the flag)

       putNonidemp = do
         setPutFlag q -- publish our intent to modify the LVar
         ticket    <- readForCAS handlerStatus
         case peekTicket ticket of
           Dormant -> execPut
           Installing n ps -> do
             (success, _) <- casIORef handlerStatus ticket $
                                      Installing n ((ClosedPar (body k)):ps)
             clearPutFlag 
             if success then sched q else putNonidemp

   in if Sched.idemp q then putIdemp else putNonidemp
