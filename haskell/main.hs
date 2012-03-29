module Main where

import Control.Monad.State
import Control.Monad.Trans
import qualified Data.Map as Map
import System.Environment (getArgs)

import GameRandom
import GameStrategy
import Hangman
import Types
import WordsOp

main :: IO ()
main = do
       args <- getArgs
       WDS ws wm <- defaultWords
       rs        <- randomRange (testWordsNum args) (length ws)
       let words = [ ws !! i | i <- rs ]
       ys        <- mapM (playgame' wm) words
       print $ "DONE: " ++ show (length $ filter (== GAME_WON) ys) ++ "/" ++ show (length words)
     where testWordsNum :: [String] -> Int
           testWordsNum []    = 2
           testWordsNum (i:_) = read i


-- | Build a Strategy base on words which have same length to secret word. 
buildSimpleStrategy :: [EnglishWord] -> SimpleStrategy
buildSimpleStrategy xs = SimpleStrategy letters words ' '
                         where letters = buildListLetterFrequency $ buildMapWordsFrequency xs
                               words = xs

-- |  Run
playgame' :: Map.Map Int [EnglishWord] -> SecretWord -> IO GameStatus
playgame' map sw = case Map.lookup (length sw) map of
                     Just xs -> evalStateT ( run (buildSimpleStrategy xs) ) (initHG sw)
                     Nothing -> return GAME_LOST
  
-- | FIXME: non-deminited if keeping guessing same letter
run :: SimpleStrategy -> HangmanGame GameStatus
run gs = do
           hg <- get
           case gameStatus hg of
             KEEP_GUESSING -> do
                              (guess, updatedStrategy) <- lift $ runStateT (nextGuess hg) gs
                              --liftIO $ print "===========================++"
                              --liftIO $ print guess
                              --liftIO $ print updatedStrategy
                              makeGuess guess
                              logHangmanGame
                              run updatedStrategy
             x             -> return x

logHangmanGame :: HangmanGame ()
logHangmanGame = do
    hg <- get
    liftIO $ print $ displayGame hg
