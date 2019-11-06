/* global exports, require */
/* jshint -W097 */

"use strict";

exports.refEq = function(a) {
  return function(b) {
    return a == b;
  };
};
