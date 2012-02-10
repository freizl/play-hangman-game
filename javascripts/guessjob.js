/**
 * play hangman game either guess a letter or a word.
 */


GuessLetter.prototype.makeGuess = function (hangmanGame) {
   hangmanGame.guessLetter(this.guess);
};

function GuessLetter(guess) {
	this.guess = guess;
};

GuessWord.prototype.makeGuess = function (hangmanGame) {
   hangmanGame.guessWord(this.guess);
};

function GuessWord(guess) {
	this.guess = guess;
};

exports.GuessLetter = GuessLetter;
exports.GuessWord = GuessWord;
