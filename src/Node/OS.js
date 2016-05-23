"use strict";

// module Node.OS

exports.hostname = function(){
  var os = require("os");
  return os.hostname();
};
