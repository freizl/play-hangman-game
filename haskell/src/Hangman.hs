module Hangman where

import qualified Data.Set as S
import Data.List (intercalate)

import Types

-- | Representation of hangman game
printGame :: Hangman -> IO ()
printGame hg = print $ intercalate ";" 
               $ map (\ fn -> fn hg) [ guessedSoFar , show . gameScore , show . gameStatus ]

-- | A dummy hangman game
dummyHG = Hangman "" defaultMaxWrongGuess "" (S.fromList []) (S.fromList []) (S.fromList [])

initHG :: SecretWord -> Hangman
initHG sw = Hangman sw defaultMaxWrongGuess (replicate (length sw) ml) (S.fromList []) (S.fromList []) (S.fromList [])

-- | Game score calculation
gameScore :: Hangman -> Int
gameScore hg 
  | gameStatus hg == GAME_LOST = 25
  | otherwise                  = gameWrongGuessesMade hg + S.size (correctGuessedLetters hg)

-- | Number of wrong guess made so far
gameWrongGuessesMade :: Hangman -> Int
gameWrongGuessesMade hg =  S.size (incorrectGuessedLetters hg)
                         + S.size (incorrectGuessedWords hg)

-- | Current game status
gameStatus :: Hangman -> GameStatus
gameStatus hg
  | guessedSoFar hg == secretWord hg            = GAME_WON
  | gameWrongGuessesMade hg > maxWrongGuesses hg = GAME_LOST                              
  | otherwise                                   = KEEP_GUESSING       

ml :: Letter
ml = '-'

defaultMaxWrongGuess :: Int
defaultMaxWrongGuess = 5
