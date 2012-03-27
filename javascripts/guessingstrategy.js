var gj = require("./guessjob.js");
var ss = require("./simpleset.js");

/**
 * generate next guess for a hangman game.
 */
GuessingStrategy.prototype.nextGuess = function (game) {
    var me = this;
    me.buildWords(game.correctlyGuessedLetters,
                  game.incorrectlyGuessedLetters,
                  game.getGuessedSoFar());
    var nl = me.nextLetter();
    
    // TODO: this could be improved.
    // e.g. what is the best strategy when remaining guess chance is 2
    //      and possibleWords size is 4
    if(me.possibleWords.length > 0 
       && (game.numWrongGuessesRemaining() >= me.possibleWords.length
          || game.numWrongGuessesMade() >= 3)){
        return new gj.GuessWord(me.nextWord());
    } else if (nl) {
        me.lastLetter = nl;
        return new gj.GuessLetter(nl);
    }
    return 0;
};

GuessingStrategy.prototype.nextLetter = function () {
    return this.letters.shift();
};

GuessingStrategy.prototype.nextWord = function () {
    return this.possibleWords.shift();
};

GuessingStrategy.prototype.filterByGuessSoFar = function (gsf) {
    var me = this;
    gsf = new RegExp(gsf.toLowerCase());
    return me.possibleWords.filter(function (x) { return x.match(gsf); });
};

/**
 * Narrow down possible words for guessing.
 */
GuessingStrategy.prototype.buildWords = function (correctlyGuessedLetters, incorrectlyGuessedLetters, gsf) {
    var me = this;

    // include all words that contains the correct guessed letter.
    if(correctlyGuessedLetters.has(me.lastLetter)) {
        if(me.possibleWords && me.possibleWords.length > 0) {
            me.possibleWords = me.possibleWords.union(me.allPossibleWordsHash[me.lastLetter]);
        } else {
            me.possibleWords = me.allPossibleWordsHash[me.lastLetter];
        }
    }
    // exclude all words that contains the incorrect guessed letter.
    if(incorrectlyGuessedLetters.has(me.lastLetter)) {
        me.possibleWords = me.possibleWords.arrayDiff(me.allPossibleWordsHash[me.lastLetter]);
    }

    // include all words that match pattern of guessed so far.
    if(me.possibleWords && me.possibleWords.length > 0) {
        // turn out filter by reg improve performance at certain percentage.
        if(correctlyGuessedLetters.size() > 0 || incorrectlyGuessedLetters.size() > 0) {
            // TODO: could be more precisely cause both list can be non-empty
            if(incorrectlyGuessedLetters.size() > 0) {
                gsf = gsf.replace(/-/g, "[^" + incorrectlyGuessedLetters.xs.join("") + "]");
            } else {
                gsf = gsf.replace(/-/g, ".");
            }
            me.possibleWords = me.filterByGuessSoFar(gsf);
        }
        // rebuild all possible words hash and letters by frequency.
        me.allPossibleWordsHash = me.wordsFrequencyFun(me.possibleWords);
        me.letters = me.lettersByWordFrequencyFun(me.allPossibleWordsHash);

        me.letters = me.letters.arrayDiff(correctlyGuessedLetters.xs);
        me.letters = me.letters.arrayDiff(incorrectlyGuessedLetters.xs);
    }

};

function GuessingStrategy (allPossibleWords, wordsFrequencyFun, lettersByWordFrequencyFun) {
    this.vowel = ["a","o","e","u","i","d","h","t","n","s"];
    this.letters = this.vowel;
   
    this.lettersByWordFrequencyFun = lettersByWordFrequencyFun;
    this.wordsFrequencyFun = wordsFrequencyFun;
    //FIXME: duplicated with line 75.
    this.allPossibleWordsHash = wordsFrequencyFun(allPossibleWords);
    this.letters = lettersByWordFrequencyFun(this.allPossibleWordsHash);

    this.possibleWords = [];
    this.lastLetter = "";

}

exports.GuessingStrategy = GuessingStrategy;
