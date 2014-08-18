import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.Generic (addHandler, freeze)
import Data.LVar.PureSet

traverse :: HasPut e => G.Graph -> Int -> Par e s (ISet s Int)
traverse g startNode = do
  seen <- newEmptySet
  h <- newHandler seen
       (\node -> do
           mapM (\v -> insert v seen)
             (neighbors g node)
           return ())
  insert startNode seen -- Kick things off
  return seen

main = print (runParThenFreeze (traverse myGraph (0 :: G.Vertex)))

