"use strict";

//module App.Client.Router

exports.setHash = function setHash(s){
  return function(){
    window.location.hash = s;
  };
};

exports.getHash = getHash;
function getHash(){
  return window.location.hash;
}

exports.hashChanged = function hashChanged(handler) {
  return function() {
    window.addEventListener('hashchange', function() {
      var newHash = getHash();
      handler(newHash)();
    });
  };
};
