"use strict";

// module Node.OS

exports.hostname = function(){
  return function(){
    var os = require("os");
    return os.hostname();
  };
};
