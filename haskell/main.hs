module Main where

import Control.Monad.State
import qualified Data.Map as Map
import Control.Monad.Trans

import WordsOp
import Hangman
import Types
import GameStrategy

main :: IO ()
main = do
       WDS ws wm <- defaultWords
       let word        = "allopatric" --ws !! 880
           wl          = length word
           wsPerLength = Map.lookup wl wm 
           game        = initHG word in
         case wsPerLength of
           Just xs -> do
                      g <- playgame (buildSimpleStrategy xs) game
                      print $ displayGame g
           Nothing -> print "data error"

-- | Build a Strategy base one words which have same length to secret word. 
buildSimpleStrategy :: [EnglishWord] -> SimpleStrategy
buildSimpleStrategy xs = SimpleStrategy letters words ' '
                         where letters = buildListLetterFrequency $ buildMapWordsFrequency xs
                               words = xs

-- |  Run
playgame :: SimpleStrategy -> HangmanGame -> IO HangmanGame
playgame  = execStateT . run

-- | FIXME: non-deminited if keeping guessing same letter
run :: SimpleStrategy -> HangmanGameState
run gs = do
           hg <- get
           case gameStatus hg of
             KEEP_GUESSING -> do
                              (guess, updatedStrategy) <- lift $ runStateT (nextGuess hg) gs
                              liftIO $ print "===========================++"
                              liftIO $ print guess
                              liftIO $ print updatedStrategy
                              makeGuess guess
                              newhg <- get
                              liftIO $ print $ displayGame newhg
                              run updatedStrategy
             x             -> return x



{-
  TODOs
-}

-- | FIXME: log
--type GameLogWriter = WriterT [String] HangmanGameState ()



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
