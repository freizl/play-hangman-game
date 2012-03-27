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

type StrategyState a = StateT a IO NextGuess
type SimpleStrategyState = StateT SimpleStrategy IO NextGuess

nextGuess :: HangmanGame -> SimpleStrategyState
nextGuess hg = do
    s1 <- get
    modify (updateNextGuessWords hg)
    ss@(SimpleStrategy cl cw ll) <- get
    --liftIO $ print $ (show $ gameWrongGuessesMade hg) ++ "," ++ (show $ length cw)
    if (gameWrongGuessesMade hg + length cw <= 5) || gameWrongGuessesMade hg >= 3 then
      put (SimpleStrategy cl (tail cw) ll) >> return (GW $ head cw)
    else
      return (GL ll)

-- | Narrow down possible words for guessing.
updateNextGuessWords :: HangmanGame -> SimpleStrategy -> SimpleStrategy
updateNextGuessWords (HG _ _ gsf ils cls iws) (SimpleStrategy cl cw ll) =
    let possibles = fetchWordsPerLetter ll (buildMapWordsFrequency cw)
        i         = S.toList ils
        c         = S.toList cls
        cw1       = correctGuess ll c i cw possibles
        cw2       = filterPossiblesPerGuessed i c gsf cw1
        ls        = if (length cw2 > 0) then ((buildListLetterFrequency $ buildMapWordsFrequency cw2) \\ i) \\ c else cl
      in
      (SimpleStrategy (tail ls) cw2 (head ls))
        
fetchWordsPerLetter :: Char -> Map.Map Char [a] -> [a]
fetchWordsPerLetter lastLetter m
  | Map.null m        = []
  | lastLetter == ' ' = []
  | otherwise         = case Map.lookup lastLetter m of
                          Just xs -> xs
                          Nothing -> []

-- | `origins` having all possible words at initilization.
correctGuess lastLetter corrects incorrects origins possibles
  | lastLetter `elem` corrects && length origins > 0     = origins `intersect` possibles
  | lastLetter `elem` corrects && length origins == 0    = possibles
  | lastLetter `elem` incorrects                         = origins \\ possibles  
  | otherwise                                            = origins

filterPossiblesPerGuessed [] [] _ p = p
--filterPossiblesPerGuessed [] corrects guessedSoFar p = 
--    let reg = subRegex (mkRegex "-") guessedSoFar "." in
--    filter (match reg) p
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
