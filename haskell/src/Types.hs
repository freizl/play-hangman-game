{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as S

type EnglishWord    = String
type Letter         = Char
type SecretWord     = EnglishWord

type GuessedSoFar   = [Letter]
type GuessedLetters = S.Set Letter
type GuessedWords   = S.Set EnglishWord

type WordsPerLength = Map.Map Int [EnglishWord]
type WordDataSet    = ([EnglishWord], WordsPerLength)

data GameStatus  = GAME_WON | GAME_LOST | KEEP_GUESSING
                   deriving (Show, Eq)

-- | FIXME: secretWord and maxWrongGuesses are inmutable thus separate them out.??
data Hangman = Hangman { secretWord              :: SecretWord
                       , maxWrongGuesses         :: Int
                       , guessedSoFar            :: GuessedSoFar
                       , incorrectGuessedLetters :: GuessedLetters
                       , correctGuessedLetters   :: GuessedLetters
                       , incorrectGuessedWords   :: GuessedWords
                       }
               deriving (Show)

-- | anything that be able to make guess (i.e. instances of GuessAction)
data NextGuess       = forall a. GuessAction a => NextGuess {getGuess :: a}

type HangmanGame     = StateT Hangman IO

-- | Game Strategy. A parametric state `s`; computation result is `NextGuess`
type StrategyState s = StateT s IO NextGuess

-- | make guess thus Hangman would be updated.
class GuessAction g where
  makeGuess :: g -> Hangman -> Hangman

-- | Generate next guess `NextGuess` as well as update the guess strategy
class GameStategy s where
  nextGuess :: Hangman -> StrategyState s


{-
class HangmanC a where {}
instance HangmanC Hangman
-}
