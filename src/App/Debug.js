"use strict";

//module App.Debug
/*eslint-env node*/

exports.debug = function debug(a){
  console.log(JSON.stringify(a));
  return a;
};
