"use strict";

exports.intercalate = function (separator) {
  return function (xs) {
    var len = xs.length;
    if (len === 0) return "";

    var res = xs[0];
    for (var i = 1; i < len; i++) {
      res = res + separator + xs[i];
    }
    return res;
  };
};
