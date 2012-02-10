
/**
 * groupBy :: [a] -> fun -> [[a]]
 * @param xs is Array
 * @param fn is Function
 */
function groupBy (xs, fn) {
   var result = {};
   if(!fn) {
      fn = identity;
   }
   xs.forEach(function (obj) {
                 var key = fn(obj);
                 if(!result.hasOwnProperty(key)) {
                    result[key] = [];
                 }
                 result[key].push(obj);
              });
   return getObjectValues(result);
}

/**
 * Get all values into a list from a object.
 */
function getObjectValues (object) {
   var values = [],
                property;
   for (property in object) {
      if (object.hasOwnProperty(property)) {
         values.push(object[property]);
      }
   }
   return values;
}

function identity (x) {
   return x;
}

/**
 * list difference.
 * (xs ++ ys) `arrayDiff` xs == ys
 * @pre xs.length >= 0
 * @pre ys.length >= 0
 */
function arrayDiff (xs, ys) {
   xs = xs || [];
   ys = ys || [];
   return xs.filter(function (x) {
                       return ys.indexOf(x) === -1;
                    });
}

/**
 * (xs ++ ys) `union` xs == xs
 */
function union (xs, ys) {
    return xs.filter(function (x) {
                         return ys.indexOf(x) >= 0;
                     });
}

if(!Array.prototype.groupBy) {
   Array.prototype.groupBy = function (fn) {
      return groupBy(this, fn);
   };
}

if(!Array.prototype.arrayDiff) {
   Array.prototype.arrayDiff = function (ys) {
      return arrayDiff(this, ys);
   };
}

if(!Array.prototype.union) {
   Array.prototype.union = function (ys) {
      return union(this, ys);
   };
}
