traverse g startNode = do
  seen <- newEmptySet
  h <- newHandler seen
       (\node -> do
           mapM (\v -> insert v seen) (neighbors g node)
           return ())
  insert startNode seen -- Kick things off
  quiesce h
  freeze seen
