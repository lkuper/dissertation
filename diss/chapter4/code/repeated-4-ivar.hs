import Control.Monad.Par

p :: Par Int
p = do
  num <- new
  fork (put num 4)
  fork (put num 4)
  get num

main = print (runPar p)
