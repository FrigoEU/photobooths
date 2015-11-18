"use strict";

//module App.Client.Components.FileInput

exports.name = function name(f){
  return f.name;
};

exports.firstFileImpl = function firstFileImpl(nothing){
  return function(just){
    return function firstFile(ev){
      var f, files = ev.target.files;
      if(files){
        f = files[0];
        return f ? just(f) : nothing;
      } else {
        return nothing;
      }
    };
  };
};
