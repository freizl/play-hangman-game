{-# LANGUAGE TypeSynonymInstances, Rank2Types,FlexibleInstances #-}
module Hangman where

import qualified Data.Set as S
import Data.List (intercalate)
import Control.Monad.State

import Types

-- | Representation of hangman game
displayGame :: Hangman -> String
displayGame hg = intercalate ";" 
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

{-
  Play the game via either guess a letter or a word
-}

-- | Play the game by guessing a letter thus game status would be updated.
guessLetter :: Letter -> Hangman -> Hangman
guessLetter l game = let sw                 = secretWord game
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
guessWord :: EnglishWord -> Hangman -> Hangman
guessWord w game = let sw                 = secretWord game
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

{-
  Guess Job
-}
--type HangmanState = State Hangman GameStatus
type HangmanGame = StateT Hangman IO

class Guess a where
  makeGuess :: a -> HangmanGame GameStatus
  
{- 
  make a guess either by guessing a letter or a word
-}
instance Guess NextGuess where
  makeGuess (GL l) = do
                     s <- get
                     let newgame = guessLetter l s in
                       put newgame >> return (gameStatus newgame)
                       
  makeGuess (GW w) = do 
                     s <- get
                     let newgame = guessWord w s in
                       put newgame >> return (gameStatus newgame)
