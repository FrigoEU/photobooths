"use strict";

//module Server.Core
/*eslint-env node*/

exports.makeApp = function makeApp(){
  var app = require("express")();
  // var bodyParser = require("body-parser");
  // app.use(bodyParser.json());
  return app;
};

exports.listen = function listen(app){
  return function(port){
    return function(){
      var server = require("http").createServer(app);
      server.listen(port);
      return server;
    };
  };
};

exports.get = function get(app){
  return function (url){
    return function(mware){
      return function(handler){
        return function(){
          app.get(url, mware, function(req, res){
            return handler(req)(res)();
          });
        };
      };
    };
  };
};

exports.post = function get(app){
  return function (url){
    return function(mware){
      return function(handler){
        return function(){
          app.post(url, mware, function(req, res){
            return handler(req)(res)();
          });
        };
      };
    };
  };
};

exports.put = function get(app){
  return function (url){
    return function(mware){
      return function(handler){
        return function(){
          app.put(url, mware, function(req, res){
            return handler(req)(res)();
          });
        };
      };
    };
  };
};

var bodyParser = require("body-parser");
exports.jsonParser = bodyParser.json();
exports.jpegParser = bodyParser.raw({type: "jpg", limit: "5MB"});

exports.mkConvert = function mkConvert(constr){
  return function convert(req){
    return constr(req.url)(!req.body || Object.keys(req.body).length === 0 ? null : req.body)(req.params)(req.path)(req.query)(req.headers);
  };
};

exports.mkBufferConvert = function mkBufferConvert(constr){
  return function convert(req){
    return constr(req.url)(req.body)(req.params)(req.path)(req.query)(req.headers);
  };
};

exports.sendStr = function sendStr(res){
  return function(string){
    return function(){
      res.send(string);
    };
  };
};
exports.sendFile = function sendFile(res){
  return function(string){
    return function(){
      res.sendFile(string, {root: process.cwd()});
    };
  };
};
