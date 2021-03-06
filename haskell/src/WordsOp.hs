module WordsOp
  ( defaultWords
  , buildMapByLength
  , buildListLetterFrequency
  , buildMapWordsFrequency
  ) where

import Control.Monad (liftM)
import Data.Function (on)
import Data.List (groupBy, sortBy, sort)
import Data.Tuple (swap)
import System.IO
import qualified Data.Map as Map
import System.Directory (doesFileExist)

import Types

defaultWordsFile :: FilePath
defaultWordsFile = "../data/words.txt"

-- | read the default words file.
defaultWords :: IO WordDataSet
defaultWords = readFileToWords defaultWordsFile

readFileToWords :: FilePath -> IO WordDataSet
readFileToWords fp = liftM wrapWordDataSet (readfile' fp)

-- | Readfile if exists otherwise return empty.
readfile' :: FilePath -> IO String
readfile' fp = do
               e <- doesFileExist fp
               if e then readFile fp else return []

wrapWordDataSet :: String -- ^ File Content
                -> WordDataSet -- ^ Wrapped data set
wrapWordDataSet [] = ([], Map.empty)
wrapWordDataSet xs = let ws = lines xs in
                     (ws, buildMapByLength ws)
                         
-- | FIXME: a little annoying that loop the groupBy result in order to get key for map.
buildMapByLength :: [EnglishWord] -- ^ a alphabet sorted words list
                 -> Map.Map Int [EnglishWord]
buildMapByLength xs = Map.fromList [ (length $ head ys, ys) | ys <- group' $ sort' xs ]
                      where sort' = sortBy (compare `on` length)
                            group' = groupBy ((==) `on` length)

englishAlp :: [Letter]
englishAlp = "abcdefghijklmnopqrstuvwxyz"

{-
 Splitted Word List by alphabet sequency.
 @param@ a sorted list that has words with same length.
 @return@ {"a": [...], "b":[...], ... }
-}
buildMapWordsFrequency :: [EnglishWord] -> MapPerAlphabet
buildMapWordsFrequency ws = Map.mapWithKey f init
                       where init = Map.fromList [ (x, []) | x <- englishAlp ]
                             f k _ = [ b | b <- ws, k `elem` b]

{-
  @param@ wordsObj :: {"a":[...], "b":[...], ...}
  @return@ list which elements are order by word frequency.
          e.g. ["c","a","b"]
-}

buildListLetterFrequency :: Map.Map Char [EnglishWord] -> [Letter]
buildListLetterFrequency v = reverse $ map fst $ sortBy (compare `on` swap) $ Map.toList (Map.map length v)
