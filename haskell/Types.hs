module Types where

import qualified Data.Map as Map
import qualified Data.Set as S

type EnglishWord    = String
type Letter         = Char
type SecretWord     = EnglishWord
type GuessedSoFar   = [Letter]
type GuessedLetters = S.Set [Letter]
type GuessedWords   = S.Set [EnglishWord]

data WordDataSet = WDS { wordsList :: [EnglishWord]  -- ^ all english words
                       , wordsMap  :: Map.Map Int [EnglishWord] -- ^ split those words by length
                       }
                   deriving (Show)

data GameStatus  = GAME_WON | GAME_LOST | KEEP_GUESSING
                   deriving (Show, Eq)

data HangmanGame = HG { secretWord :: SecretWord
                      , guessedSoFar :: GuessedSoFar
                      , incorrectGuessedLetters :: GuessedLetters
                      , correctGuessedLetters :: GuessedLetters
                      , incorrectGuessedWords :: GuessedWords
                      , maxWrongGuesses :: Int
                      }
