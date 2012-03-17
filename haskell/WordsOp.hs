{- Load words.txt
-}

module WordsOp
  ( defaultWords
  , buildMapByLength
  ) where

import Control.Monad (liftM)
import Data.Function (on)
import Data.List (groupBy)
import System.IO

import Types (EnglishWord, WordDataSet)             

defaultWordsFile :: FilePath
defaultWordsFile = "../data/words.txt"

-- | FIXME: error handle while file does not exists
defaultWords :: IO WordDataSet
defaultWords = readFileToWords defaultWordsFile

readFileToWords :: FilePath -> IO WordDataSet
readFileToWords fp = liftM wrapWordDataSet (readFile fp)

wrapWordDataSet :: String -- ^ File Content
                -> WordDataSet -- ^ Wrapped data set
wrapWordDataSet [] = WDS [] Map.empty
wrapWordDataSet xs = let ws = words xs in
                     WDS ws (buildMapByLength ws)    
                         
-- | FIXME: a little annoying that loop the groupBy result in order to get key for map.
buildMapByLength :: [EnglishWord] -- ^ a alphabet sorted words list
                 -> Map.Map Int [EnglishWord]
buildMapByLength xs = Map.fromList [ (length ys, ys) | ys <- groupBy ((==) `on` length) xs ]
