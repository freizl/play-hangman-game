var ss = require('./simpleset.js');

/**
 * Generate a few random numbers by a generator.
 * @param count how many random numbers will be generated.
 * @param randomGen the random number generator.
 */
function randomRange(count, randomGen) {
    var result = new ss.SimpleSet();
    while(result.size() < count) {
        result.add(randomGen());
    }
    return result.xs;
}

/**
 * A Random generator per its <code>upperLimit</code>.
 */
function getRandomGenerator(upperLimit) {
    return function () {
        return Math.floor(Math.random() * upperLimit);
    };
}

exports.getRandomGenerator = getRandomGenerator;
exports.randomRange = randomRange;
