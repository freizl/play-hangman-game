/**
 * A trivil Set implementation. Would be deprecated by 3rd party lib.
 */

SimpleSet.prototype.add = function (x) {
	if(!this.has(x)) {
		this.xs.push(x);
	};
};

SimpleSet.prototype.has = function (x) {
	return this.xs && this.xs.indexOf(x) >= 0;
};

SimpleSet.prototype.addAll = function (ys) {
	var me = this;
    !ys || ys.map(function (y) { me.add(y);});
};

SimpleSet.prototype.size = function () { return this.xs.length; };

function SimpleSet () {
	this.xs = [];
};

exports.SimpleSet = SimpleSet;
