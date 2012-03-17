/**
 * Transform the word list into several format.
 */

require('./arrayUtil.js');
var wordsFile = '../data/words.txt';
var encoding  = 'utf-8';
var englishAlp = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".split("");
var fs = require('fs');

var data = fs.readFileSync(wordsFile, encoding);
if (data) {
   data = data.split("\n");
   // 1 order by length ASC
   var xs = data.sort(function (a, b) { return a.length - b.length;});
   // 2 group words by same length
   var wlByLength = xs.groupBy(function (x) {return x.length;})
                      .filter(function (s) { return s.length > 0; });

   exports.wordList = data;
   exports.wordsByLength = wlByLength;
    
   exports.wordsFrequencyFun = wordsFrequencyFun;
   exports.lettersByWordFrequencyFun = lettersByWordFrequencyFun;
}

/**
 * Splitted Word List by alp sequency.
 * @param a list that has words with same length.
 * @return {"a": [...], "b":[...], ... }
 */
function wordsFrequencyFun (words) {
   var result = {};
   englishAlp.forEach(function (x) { result[x] = [];});
   words.forEach(function (w) {
                    var word = w.toUpperCase();
                    englishAlp.forEach(function (x){
                                          if(word.indexOf(x) >= 0) {
                                             result[x].push(w);
                                          }
                                       });
             });
   return result;
}

/**
 * @param wordsObj :: {"a":[...], "b":[...], ...}
 * @return list which elements are order by word frequency.
 *         e.g. ["c","a","b"]
 */
function lettersByWordFrequencyFun (wordsObj) {
    var result = [];
    for (var key in wordsObj) {
        result.push([key, wordsObj[key].length]);
    }
    return result.sort(function (x, y) { return y[1] - x[1];})
                 .map(function (x){ return x[0];});
}
