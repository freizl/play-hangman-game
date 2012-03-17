module Hangman where

import qualified Data.Set as S
import Data.List (intercalate)

import Types

instance Show HangmanGame where
  show hg = intercalate ";" 
            $ map (\ fn -> fn hg) [ guessedSoFar , show . gameScore , show . gameStatus ]

emptyHG = HG "" "" (S.fromList []) (S.fromList []) (S.fromList []) defaultMaxWrongGuess

gameScore :: HangmanGame -> Int
gameScore hg 
  | gameStatus hg == GAME_LOST = 25
  | otherwise                  = numWrongGuessesMade hg + S.size (correctGuessedLetters hg)

numWrongGuessesMade hg =  S.size (incorrectGuessedLetters hg)
                        + S.size (incorrectGuessedWords hg)

gameStatus :: HangmanGame -> GameStatus
gameStatus hg
  | guessedSoFar hg == secretWord hg            = GAME_WON
  | numWrongGuessesMade hg > maxWrongGuesses hg = GAME_LOST                              
  | otherwise                                   = KEEP_GUESSING       

mysteryLetter :: Letter
mysteryLetter = '-'

defaultMaxWrongGuess :: Int
defaultMaxWrongGuess = 5
