traverse :: Graph -> NodeLabel -> Par (Set NodeLabel)
traverse g startNode = do
  seen <- newEmptySet
  insert startNode seen -- Kick things off
  let handle node = mapM (\v -> insert v seen) (neighbors g node)
  freezeSetAfter seen handle
