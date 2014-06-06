traverse :: Graph -> NodeLabel -> Par (Set NodeLabel)
traverse g startV = do
  seen <- newEmptySet
  putInSet seen startV
  let handle node = parMapM (putInSet seen) (nbrs g node)
  freezeSetAfter seen handle
   
