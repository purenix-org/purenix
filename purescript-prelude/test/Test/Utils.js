"use strict";

exports.throwErr = function(msg) {
  return function() {
    throw new Error(msg);
  };
};
