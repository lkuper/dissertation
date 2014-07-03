#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns, RecordWildCards , ScopedTypeVariables #-}

module Main where

-- import Data.Char.ByteString
import Prelude
import Data.List
import Data.Maybe
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split
import Control.Monad

data Run = Run 
  { progname :: String
  , variant :: String
  , medtime :: Double
  , threads :: Int
  , runid   :: String
  , args    :: [String]
  } deriving (Show,Eq,Ord,Read)


groupSort fn = 
   (groupBy ((==) `on` fn)) . 
   (sortBy (compare `on` fn))

-- -- | Add three more levels of list nesting to organize the data:
-- organize_data :: [Entry] -> [[[[Entry]]]]
organize_data = 
	 (map (map (groupSort threads)))  . 
  	      (map (groupSort whichBench)) .
                   (groupSort progname)


-- For CFA, the flag tells us which benchmark it is:
whichBench Run{args= which:topo:nums, variant="none"} =
  (which, topo, nums)
whichBench oth = error$"whichBench: unexpected format: "++show oth


takeBest a@Run{medtime} b@Run{medtime=m2} | medtime <= m2 = a
                                          | otherwise     = b

setify :: Ord a => [a] -> [a]
setify = S.toList . S.fromList 

--------------------------------------------------------------------------------

main :: IO ()
main = do
  
  dat <- readFile "All_Delta_runs.csv"

  let lns  = lines dat
      splt = map (splitOn ",") lns
      cols = transpose splt
      labeled = map (\(hd:tl) -> (hd,tl)) cols

      parsed = let pname   = fromJust (lookup "PROGNAME" labeled)
                   variant = fromJust (lookup "VARIANT" labeled)
                   medtime = fromJust (lookup "MEDIANTIME" labeled)
                   threads = fromJust (lookup "THREADS" labeled)
                   runid   = fromJust (lookup "RUNID" labeled)
                   args   = fromJust (lookup "ARGS" labeled)
               in map parse (transpose [pname,variant,medtime,threads,runid,args])
      parse [progname,variant,medtime,threads,runid,args] =
           Run {progname,variant,medtime=parseDbl medtime,threads=read threads,runid,args=words args}
      parseDbl "Infinity" = 1/0
      parseDbl s          = read s

      relevant = filter (includedRunID . runid) parsed
      -- includedRunID s = elem s ["d005_1373516632","d008_1373577010"]
      includedRunID s = elem s ["d005_1373532273",
                                "d003_1373528191",
                                "d005_1373529332",
                                "d003_1373516538" -- Older, without work-variation.
                                ]
      -- includedRunID s = take 3 s == "d00"
  
  let all_bfs  = [ r | r@Run{progname} <- relevant, isInfixOf "bfs" progname ]

  let aggr results (r@Run{..}) =
        M.insertWith takeBest (whichBench r) r results
  let singThread 1 = True
      singThread 0 = True
      singThread _ = False
      observed_benches = setify$ map whichBench all_bfs
  putStrLn $"Found these benches "++show observed_benches
  let sumit list = do 
        forM observed_benches $ \ bench -> do 
          putStrLn$ "Summary for benchmark "++show bench++" speedups:"
          let suite  = filter ((==bench) . whichBench) list
              seqCandidates = filter (singThread . threads) suite

          if (null seqCandidates)
            then do putStrLn "WARNING: No seq candidates!"
                    return (0,bench)
            else do
            
            let seqRun = foldl1 takeBest seqCandidates
            -- print seqRun
            when (length seqCandidates > 1) $ do 
              putStrLn "WARNING: Multiple seq candidates: "
              mapM_ print seqCandidates

            putStrLn$ "  SequentialTime: "++ show(medtime seqRun)
            ups <- forM (setify$ map threads suite) $ \ numThreads -> do
              let onerun = foldl1 takeBest $
                           filter ((==numThreads) . threads) suite
              let speedup = medtime seqRun / medtime onerun 
              putStrLn$ "  Threads="++show numThreads++": "++show speedup ++ " rawtime "++show (medtime onerun)
              return (speedup)
            return (foldl max 0 ups, bench)
  
  all_ups <- sumit all_bfs

  putStrLn$ "Best speedups, sorted: "
  forM_ (sortBy (compare `on` fst) all_ups) print
--  putStrLn$ "lockfree: "++show (filter (isInfixOf "lockfree") lns)
--  putStrLn$ "lockfree: "++show (filter ((=="inplace_lockfree") . variant) parsed)
--  mapM_ print cfaBaseline
--  print $ organize_data parsed

  return ()
