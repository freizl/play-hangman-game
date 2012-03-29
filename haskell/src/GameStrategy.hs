{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}

module GameStrategy where

import Text.Regex
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as S
import Control.Monad.Trans
import Data.List

import Types
import WordsOp
import Hangman
import Guess

{-
  TODO: 1. failure at when define 
           `class (Guess g, HangmanC h) => GuessAction g h | g -> h where ...`
-}


data SimpleStrategy = SimpleStrategy { candidateLetters :: [Letter]
                                     , candidateWords   :: [EnglishWord]
                                     , lastLetter       :: Letter
                                     , mapAlphabet      :: MapPerAlphabet
                                     }

-- | Build a Strategy base on words which have same length to secret word. 
newSimpleStrategy :: [EnglishWord] -> SimpleStrategy
newSimpleStrategy xs = SimpleStrategy letters words ' ' map
  where map = buildMapWordsFrequency xs
        letters = buildListLetterFrequency map
        words = []

instance Show SimpleStrategy where
  show (SimpleStrategy ls ws ll _) = ll : "->" ++ show ls ++ show (take 5 ws)

instance GameStategy SimpleStrategy where
  nextGuess hg = do
    s1 <- get
    modify (updateNextGuessWords hg)
    ss@(SimpleStrategy cl cw ll map) <- get
    -- FIXME: complicated if-else thus need lang ext DoAndIfThenElse. can be simple??
    liftIO $ print $ take 5 cw
    if (length cw > 0) && ((gameWrongGuessesMade hg + length cw <= 5) || gameWrongGuessesMade hg >= 3) then
      put (SimpleStrategy cl (tail cw) ll map) >> return (NextGuess $ GuessWord $ head cw)
    else
      return (NextGuess $ GuessLetter ll)
  
-- | Narrow down possible words for guessing.
updateNextGuessWords :: Hangman -> SimpleStrategy -> SimpleStrategy
updateNextGuessWords (Hangman _ _ gsf ils cls iws) (SimpleStrategy cl cw ll map) =
    let possibles = fetchWordsPerLetter ll map
        i         = S.toList ils
        c         = S.toList cls
        cw1       = correctGuess ll c i cw possibles
        cw2       = filterPossiblesPerGuessed i c gsf cw1
        map1      = if length cw2 > 0 then buildMapWordsFrequency cw2 else map
        ls        = if (length cw2 > 0) then ((buildListLetterFrequency map1) \\ i) \\ c else cl
      in
      (SimpleStrategy (tail ls) cw2 (head ls) map1)

        
fetchWordsPerLetter :: Char -> Map.Map Char [a] -> [a]
fetchWordsPerLetter lastLetter m
  | Map.null m        = []
  | lastLetter == ' ' = []
  | otherwise         = case Map.lookup lastLetter m of
                          Just xs -> xs
                          Nothing -> []

correctGuess lastLetter corrects incorrects origins possibles
  | lastLetter `elem` corrects && length origins > 0     = origins `union` possibles
  | lastLetter `elem` corrects && length origins == 0    = possibles
  | lastLetter `elem` incorrects                         = origins \\ possibles  
  | otherwise                                            = origins

filterPossiblesPerGuessed [] [] _ p = p
filterPossiblesPerGuessed incorrects corrects guessedSoFar p = 
    let neg = if length incorrects > 0 then ("[^" ++ incorrects ++ "]") else "."
        reg = subRegex (mkRegex "-") guessedSoFar neg in
    filter (match reg) p

match :: String      -- ^ reg exp
       -> EnglishWord -- ^ test word
       -> Bool        -- ^ <==> Ture if match
match s w = case matchRegex (mkRegex s) w of       
              Just _  -> True
              Nothing -> False
