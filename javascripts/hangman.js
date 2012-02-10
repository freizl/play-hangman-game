/**
 * The main route of a hangman game. Copy from the Java impl basically.
 */

var sm = require("./simpleset.js");

var status = {GW: "GAME_WON", GL: "GAME_LOST", KG: "KEEP_GUESSING"};

HangmanGame.prototype.guessLetter = function (ch) {
  ch = ch.toUpperCase();

  var goodGuess = false;
  for (var i = 0; i < this.secretWord.length; i++) {
    if (this.secretWord.charAt(i) == ch) {
      this.guessedSoFar[i] = ch;
      goodGuess = true;
    }
  }
  
  if (goodGuess) {
    this.correctlyGuessedLetters.add(ch);
  } else {
    this.incorrectlyGuessedLetters.add(ch);
  }

  return this.getGuessedSoFar();
};

HangmanGame.prototype.guessWord = function (guess) {
  guess = guess.toUpperCase();

  if (guess === this.secretWord) {
    for (var i = 0; i<this.secretWord.length; i++) {
      this.guessedSoFar[i] = this.secretWord.charAt(i);
    }
  } else {
    this.incorrectlyGuessedWords.add(guess);
  }

  return this.getGuessedSoFar();
};

/**
 * @return The score for the current game state
 */
HangmanGame.prototype.currentScore = function () {
  var me = this;
  if (me.gameStatus() === me.STATUS.GL) {
    return 25;
  } else {
    return me.numWrongGuessesMade() + me.correctlyGuessedLetters.size();
  }
};

HangmanGame.prototype.canKeepGuessing = function () {
    return this.gameStatus() == this.STATUS.KG;
};

/**
 * @return The current game status
 */
HangmanGame.prototype.gameStatus = function () {
  var me = this;
  if (me.secretWord === me.getGuessedSoFar()) {
    return me.STATUS.GW;
  } else if (me.numWrongGuessesMade() > me.maxWrongGuesses) {
    return me.STATUS.GL;
  } else {
    return me.STATUS.KG;
  }
};

HangmanGame.prototype.numWrongGuessesMade = function () {
  return this.incorrectlyGuessedLetters.size() + this.incorrectlyGuessedWords.size();
};

HangmanGame.prototype.numWrongGuessesRemaining = function () {
  return this.maxWrongGuesses - this.numWrongGuessesMade();
};


HangmanGame.prototype.getGuessedSoFar = function () {
  return this.guessedSoFar.join("");
};

/**
 * @return The length of the secret word
 */
HangmanGame.prototype.getSecretWordLength = function () {
  return this.secretWord.length;
};

HangmanGame.prototype.toString = function () {
  return this.getGuessedSoFar() + "; score=" + this.currentScore() + "; status=" + this.gameStatus();
};

/**
  * @param secretWord
  * @param maxWrongGuesses
  */
function HangmanGame (secretWord, maxWrongGuesses) {
  this.STATUS = status;
  this.MYSTERY_LETTER = '-';

  this.guessedSoFar = [];
  this.correctlyGuessedLetters = new sm.SimpleSet();
  this.incorrectlyGuessedLetters = new sm.SimpleSet();
  this.incorrectlyGuessedWords = new sm.SimpleSet();

  this.secretWord = secretWord.toUpperCase();
  for (var i = 0; i < secretWord.length; i++) {
    this.guessedSoFar[i] = this.MYSTERY_LETTER;
  }
  this.maxWrongGuesses = maxWrongGuesses || 5;
}

exports.HangmanGame = HangmanGame;
exports.status = status;
