"use strict";

//module Data.Int.Extended

exports.pInt = function pInt(nothing){
  return function(just){
    return function(str){
      var i = parseInt(str);
      if (isNaN(i)){
        return nothing;
      } else {
        return just(i);
      }
    };
  };
};
