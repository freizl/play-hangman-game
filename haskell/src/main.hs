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
       args      <- getArgs
       (ws, wm)  <- defaultWords
       rs        <- randomRange (testWordsNum args) (length ws)
       let words = [ ws !! i | i <- rs ]
       ys        <- mapM (playgame' wm) words
       print $ "DONE: " ++ show (length $ filter (== GAME_WON) ys) ++ "/" ++ show (length words)
     where testWordsNum :: [String] -> Int
           testWordsNum []    = 2
           testWordsNum (i:_) = read i

-- |  Run
playgame' :: MapPerLength -> SecretWord -> IO GameStatus
playgame' map sw = case Map.lookup (length sw) map of
                     Just xs -> evalStateT ( run (newSimpleStrategy xs) ) (initHG sw)
                     Nothing -> return GAME_LOST
  
-- | FIXME: non-deminited if keeping guessing same letter
run :: SimpleStrategy -> HangmanGame GameStatus
run gs = do
           hg <- get
           case gameStatus hg of
             KEEP_GUESSING -> do
                              (NextGuess guess, updatedStrategy) <- runStrategy hg gs
                              put (makeGuess guess hg)
                              logHangmanGame
                              run updatedStrategy
             x             -> return x
        where runStrategy hg gs = lift $ runStateT (nextGuess hg) gs
              logHangmanGame = do hg <- get
                                  liftIO $ print $ displayGame hg

