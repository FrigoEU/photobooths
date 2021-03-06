"use strict";

//module Server.Core
/*eslint-env node*/

exports.makeApp = function makeApp(){
  var app = require("express")();
  var compression = require("compression");
  app.use(compression());
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
          var cb = function(req, res){ return handler(req)(res)(); };
          var args = mware ? [url, mware, cb] : [url, cb];
          app.get.apply(app, args);
        };
      };
    };
  };
};
exports.delete = function get(app){
  return function (url){
    return function(mware){
      return function(handler){
        return function(){
          var cb = function(req, res){ return handler(req)(res)(); };
          var args = mware ? [url, mware, cb] : [url, cb];
          app.delete.apply(app, args);
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
          var cb = function(req, res){ return handler(req)(res)(); };
          var args = mware ? [url, mware, cb] : [url, cb];
          app.post.apply(app, args);
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
          var cb = function(req, res){ return handler(req)(res)(); };
          var args = mware ? [url, mware, cb] : [url, cb];
          app.put.apply(app, args);
        };
      };
    };
  };
};

exports.hostStatic = function(app){
  return function(root){
    return function(){
      app.use(require("express").static(root));
    };
  };
};

var bodyParser = require("body-parser");
exports.jsonParser = bodyParser.json();
exports.bufferParser = bodyParser.raw({type: "*/*", limit: "5MB"});
exports.rawParser = bodyParser.text({type: "*/*"});
exports.noParser = null;

exports.mkConvert = function mkConvert(constr){
  return function(unit){
    return function convert(req){
      return constr(req.url)(!req.body ? unit : req.body)(req.params)(req.path)(req.query)(req.headers);
    };
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
exports.sendBuffer = function sendBuffer(res){
  return function(buffer){
    return function(){
      res.send(buffer);
    };
  };
};
exports.getParamsImpl = function getParamsImpl(just){
  return function(nothing){
    var regex = /\?params=(.*)$/;
    return function(url){
      var matches = url.match(regex);
      return matches[1] ? just(matches[1]) : nothing;
    };
  };
};
