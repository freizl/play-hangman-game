module Main where

import Control.Monad.State
import qualified Data.Map as Map
import Control.Monad.Trans

import WordsOp
import Hangman
import Types

main :: IO ()
main = do
       WDS ws wm <- defaultWords
       let word        = "allopatric" --ws !! 880
           wl          = length word
           wsPerLength = Map.lookup wl wm 
           game        = initHG word in
         case wsPerLength of
           Just xs -> print $ displayGame $ playgame (buildSimpleStrategy xs) game
           Nothing -> print "data error"

-- | Build a Strategy base one words which have same length to secret word. 
buildSimpleStrategy :: [EnglishWord] -> SimpleStrategy
buildSimpleStrategy xs = SimpleStrategy letters words
                         where letters = lettersByWordsFrequency $ wordsFrequencyMap xs
                               words = []

-- | FIXME: is it possible to do : `forall a. State a NextGuess
--type DummyStrategyState = State DummyStrategy NextGuess  
type StrategyState = State SimpleStrategy NextGuess

-- | Fetch next guess per current hame and update the strategy state
nextGuess' :: HangmanGame -> StrategyState
nextGuess' hg = do
                s <- get
                let ls = candidateLetters s
                    ws = candidateWords s
                    l = head ls
                    r = tail ls in
                  put (SimpleStrategy r ws) >> return (GL l)

-- |  Run
playgame :: SimpleStrategy -> HangmanGame -> HangmanGame
playgame  = execState . run 

-- | FIXME: non-deminited if keeping guessing same letter
run :: SimpleStrategy -> HangmanGameState
run gs = do
           hg <- get
           case gameStatus hg of
             KEEP_GUESSING -> let (guess, updatedStrategy) = runState (nextGuess' hg) gs in
                              makeGuess guess >> run updatedStrategy
             x             -> return x



{-
  TODOs
-}

-- | FIXME: log
--type GameLogWriter = Writer GameLog ()



--class GameStrategy s where
--  nextGuess :: HangmanGame -> s
--  
--instance GameStrategy DummyStrategyState where
--  nextGuess hg = do
--                s <- get
--                let ls = getLetters s
--                    l = head ls
--                    r = tail ls in
--                  put (DummyStrategy r) >> return (GL l)
