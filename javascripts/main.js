var hm = require("./hangman.js");
var gs = require("./guessingstrategy.js");
var words = require("./words.js");
var ran = require('./random.js');

var oneWord = process.argv[2];

var ranWords = [];
if (oneWord) {
    ranWords = [oneWord];
} else {
    ranWords = genRandomWords();
}
var result = ranWords.map(playHangman);
console.log(">>> RESULT:", result.filter(function (x) {return x===hm.status.GW;}).length , "/",result.length);

function playHangman (word, maxWrong) {
    //console.log(">>> the secret word is:", word);
    maxWrong = maxWrong || 5;
    var g = new hm.HangmanGame(word, maxWrong);
    var swLength = g.getSecretWordLength();
    var s = new gs.GuessingStrategy(words.wordsByLength[swLength-1],
                                    words.wordsFrequencyFun,
                                    words.lettersByWordFrequencyFun);
    run(g, s);
    return g.gameStatus();
}

function genRandomWords (randomCount) {
    randomCount = randomCount || 10;
    var rg = ran.getRandomGenerator(words.wordList.length);
    var xs = ran.randomRange(randomCount, rg);
    return xs.map(function (i) { return words.wordList[i]; });
}

function run (game, strategy) {
   while(game.canKeepGuessing()) {
      var nextGuess = strategy.nextGuess(game);
      if(nextGuess) {
         nextGuess.makeGuess(game);
         console.log(game.toString());
      } else {
         break;
      }
   };
};

