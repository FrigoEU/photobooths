"use strict";

//module App.Model.StrMap

exports._collect = function _collect (f) {
  return function (m) {
    var r = [];
    for (var k in m) {
      if (m.hasOwnProperty(k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
};
