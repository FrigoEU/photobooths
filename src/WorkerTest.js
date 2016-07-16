"use strict";

// module WorkerTest

exports.addTenMinutes = function(d){
  var newd = new Date(d);
  newd.setMinutes(newd.getMinutes() + 10);
  return newd;
};
