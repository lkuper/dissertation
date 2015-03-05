import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureMap
import qualified Data.Map as M

p :: (HasPut e, HasFreeze e) => Par e s (M.Map Item Int)
p = do cart <- newEmptyMap
       fork (insert Book 2 cart)
       fork (insert Shoes 1 cart)
       freezeMap cart

main = do v <- runParQuasiDet p
          print (M.toList v)
