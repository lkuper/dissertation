nbrs :: Graph -> NodeLabel -> Set NodeLabel
-- `nbrs g n' is the neighbor nodes of node `n' in graph `g'.

-- Traverse each level of the graph `g' in parallel, maintaining at each
-- recursive step a set of nodes that have been seen and a set of
-- nodes left to process.
bf_traverse :: Graph -> Set NodeLabel -> Set NodeLabel -> Set NodeLabel
bf_traverse g seen nu = 
  if nu == empty 
  then seen
  else let seen'  = union seen nu
           allNbr = parFold union (parMap (nbrs g) nu)
           nu'    = difference allNbr seen'
       in bf_traverse g seen' nu'

-- Find the connected component containing the vertex `profile0', and
-- map `analyze` over all nodes in it:
connected_component = bf_traverse profiles empty (singleton profile0)
result = parMap analyze connected_component
