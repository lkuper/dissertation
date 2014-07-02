nbrs :: Graph -> NodeLabel -> Set NodeLabel
-- `nbrs g n' is the neighbor nodes of node `n' in graph `g'.

-- Traverse each level of the graph in parallel, maintaining at each
-- recursive step a set of nodes that have been seen and a set of
-- nodes left to process.
bf_traverse :: Graph -> Set NodeLabel -> Set NodeLabel -> Set NodeLabel
bf_traverse g seen nu = 
  if nu == S.empty 
  then seen
  else let seen'  = union seen nu
           allNbr = parFold union (parMap (nbrs g) nu)
           nu'    = difference allNbr seen'
       in bf_traverse g seen' nu'

-- Next we traverse the connected component, starting with the vertex
-- `profile0':
ccmp = bf_traverse profiles S.empty     profile0Sing
result = parMap analyze ccmp
