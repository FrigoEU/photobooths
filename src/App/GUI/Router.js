"use strict";

//module App.GUI.Router

exports.setHash = function setHash(s){
  return function(){
    window.location.hash = encodeURI(s);
  };
};

exports.getHash = getHash;
function getHash(){
  return decodeURI(window.location.hash);
}

exports.hashChanged = function hashChanged(handler) {
  return function() {
    window.addEventListener('hashchange', function() {
      var newHash = getHash();
      handler(newHash)();
    });
  };
};

// exports.getBasePathImpl = function getBasePathImpl(just){
//   return function(nothing){
//     var regex = /#\/([^\/\?]*).*/;
//     return function(string){
//       var matches = string.match(regex);
//       return matches ? just(matches[1]) : nothing;
//     }
//   }
// }
