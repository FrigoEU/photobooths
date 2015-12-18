"use strict";

//module App.Model.Date
/*eslint-env node*/

exports.iso8601 = function iso8601(d){
  return d.toISOString();
};

exports.toLocalDatetime = function toLocalDatetime(d){
  return d.toISOString().substring(0, 11) + d.toString().substring(16, 21);
};
exports.fromLocalDatetimeImpl = function fromLocalDatetimeImpl(nothing){
  return function(just){
    return function(str){//YYYY-mm-DDTHH:MM
      var newD = new Date(); 
      var offset = newD.toString().substring(28, 33); //+0100
      var d = new Date(str + offset);
      if (d && d.toString() !== "Invalid Date"){
        return just(d);
      } else {
        return nothing;
      }
    };
  };
};
