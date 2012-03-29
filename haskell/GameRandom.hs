module GameRandom 
  (randomRange) where

import System.Random
import qualified Data.Set as S

type Count      = Int
type UpperLimit = Int
type RandomSet  = S.Set Int

-- | Generate `Count` size Int list.
randomRange :: Count -> UpperLimit -> IO [Int]
randomRange c u = do
                  rs <- genNewRandom c u (S.fromList [])
                  return (S.toList rs)

genNewRandom :: Count -> UpperLimit -> RandomSet -> IO RandomSet
genNewRandom c u rs 
  | S.size rs == c    = return rs
  | otherwise         = do
                        i <- getRandom u 
                        genNewRandom c u (S.insert i rs)

getRandom :: UpperLimit -> IO Int
getRandom ul = getStdRandom (randomR (1,ul))
