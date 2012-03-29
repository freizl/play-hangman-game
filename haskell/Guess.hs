module Guess where

import qualified Data.Set as S

import Types

data GuessLetter = GuessLetter Letter
data GuessWord   = GuessWord   EnglishWord

instance GuessAction GuessLetter where
  makeGuess (GuessLetter l) = guessLetter' l

instance GuessAction GuessWord where
  makeGuess (GuessWord w) = guessWord' w


-- | Play the game by guessing a letter thus game status would be updated.
guessLetter' :: Letter -> Hangman -> Hangman
guessLetter' l game = let sw                 = secretWord game
                          gs                 = zipWith (compareLetter l) sw (guessedSoFar game)
                          goodGuess          = l `elem` sw
                          -- The following 4 lines are such annoying.
                          originalIncorrects = incorrectGuessedLetters game
                          originalCorrects   = correctGuessedLetters game
                          incorrects         = if not goodGuess then S.insert l originalIncorrects else originalIncorrects
                          corrects           = if goodGuess then S.insert l originalCorrects else originalCorrects
                          -- ^
                          newGame            = game { guessedSoFar = gs, incorrectGuessedLetters = incorrects, correctGuessedLetters = corrects } in
                       newGame

-- | Play the game by guessing a word thus game status would be updated.
guessWord' :: EnglishWord -> Hangman -> Hangman
guessWord' w game = let sw                 = secretWord game
                        goodGuess          = w == sw
                        originalGs         = guessedSoFar game
                        originalIncorrects = incorrectGuessedWords game
                        -- The following lines are such annoying.
                        incorrects         = if not goodGuess then S.insert w originalIncorrects else originalIncorrects
                        gs                 = if goodGuess then w else originalGs
                        -- ^
                        newGame           = game { guessedSoFar = gs, incorrectGuessedWords = incorrects } in
                    newGame

compareLetter :: Letter -- ^ A guessing Letter
                 -> Letter -- ^ A letter from secret word
                 -> Letter -- ^ A letter from guessedSoFar
                 -> Letter -- ^ The selected one among those three
compareLetter x y z 
  | z /= '-'  = z
  | x == y    = x
  | otherwise = '-'
