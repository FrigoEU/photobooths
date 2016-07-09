"use strict";

//module App.Networking
//
var http = require('http');
var fs = require('fs');

exports._downloadFile = function(name){
  return function(url){
    return function(errcb){
      return function(successcb){
        return function(){
          var file = fs.createWriteStream(name);
          http.get(url, function(response) {
            response.pipe(file);
            file.on("finish", function() {
              file.close(function(x){
                successcb(x)();
              });
            });
          }).on("error", function(err){
            fs.unlink(file);
            errcb(err)();
          });
        };
      };
    };
  };
};
