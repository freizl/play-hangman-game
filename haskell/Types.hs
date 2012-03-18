module Types where

import qualified Data.Map as Map
import qualified Data.Set as S

type EnglishWord    = String
type Letter         = Char
type SecretWord     = EnglishWord

type GuessedSoFar   = [Letter]
type GuessedLetters = S.Set Letter
type GuessedWords   = S.Set EnglishWord

data WordDataSet = WDS { wordsList :: [EnglishWord]  -- ^ all english words
                       , wordsMap  :: Map.Map Int [EnglishWord] -- ^ split those words by length
                       }
                   deriving (Show)

data GameStatus  = GAME_WON | GAME_LOST | KEEP_GUESSING
                   deriving (Show, Eq)

-- | FIXME: secretWord and maxWrongGuesses are inmutable thus separate them out.??
data HangmanGame = HG { secretWord :: SecretWord
                      , maxWrongGuesses :: Int
                      , guessedSoFar :: GuessedSoFar
                      , incorrectGuessedLetters :: GuessedLetters
                      , correctGuessedLetters :: GuessedLetters
                      , incorrectGuessedWords :: GuessedWords
                      }
                   deriving (Show)

data NextGuess = GL Letter | GW EnglishWord


data SimpleStrategy = SimpleStrategy { candidateLetters :: [Letter]
                                     , candidateWords   :: [EnglishWord]
                                     }
                      deriving (Show)

data DummyStrategy = DummyStrategy { getLetters :: [Letter] }

newtype GameLog = GameLog [String]
