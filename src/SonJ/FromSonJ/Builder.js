/* global exports, require */
/* jshint -W097 */

"use strict";

exports.unsafeMapArray = function(f) {
  return function(arr) {
    for(var i=0; i<arr.length; i++) {
      arr[i] = f(arr[i]);
    }
    return arr;
  };
};

exports.unsafeModifyRecord = function(l) {
  return function (f) {
    return function(rec) {
      rec[l] = f(rec[l]);
      return rec;
    };
  };
};

