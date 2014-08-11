import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureMap

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: (HasPut e, HasGet e) => Par e s (IMap Item s Int)
p = do
  cart <- newEmptyMap
  fork (insert Book 2 cart)
  fork (insert Shoes 1 cart)
  return cart

main = print (runParThenFreeze p)
