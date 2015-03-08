import Control.LVish
import Control.LVish.DeepFrz -- provides Frzn
import Data.LVar.Generic (addHandler, freeze)
import Data.LVar.PureSet
import qualified Data.Graph as G

traverse :: (HasPut e, HasFreeze e) =>
            G.Graph -> Int -> Par e s (ISet Frzn Int)
traverse g startNode = do
  seen <- newEmptySet
  h <- newHandler seen
       (\node -> do
           mapM (\v -> insert v seen)
             (neighbors g node)
           return ())
  insert startNode seen -- Kick things off
  quiesce h
  freeze seen

main = do
  v <- runParQuasiDet (traverse myGraph (0 :: G.Vertex))
  print (fromISet v)
