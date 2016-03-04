"use strict";

//module App.Config

exports.requireConfigFile = function(path){
  return function(){
    return require(path);
  };
};
