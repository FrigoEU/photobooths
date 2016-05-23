module App.Exec (simpleExec) where

import Prelude ((>>=), ($), (>))

import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (StrMap)

import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Aff (Aff, makeAff)

import Node.ChildProcess (CHILD_PROCESS, execFile)
import Node.Buffer (size, toString, BUFFER)
import Node.Encoding (Encoding(UTF8))

simpleExec :: forall eff. 
              String 
              -> Maybe String
              -> Array String 
              -> Maybe (StrMap String) 
              -> Aff (cp :: CHILD_PROCESS, err :: EXCEPTION, buffer :: BUFFER | eff) String
simpleExec cmd cwd args env = 
  makeAff (\errcb okcb -> 
             let ops = { cwd: cwd
                       , env: env
                       , timeout: Nothing
                       , maxBuffer: Nothing
                       , killSignal: Nothing
                       , uid: Nothing
                       , gid: Nothing }
                 handler {error: (Just err)} = errcb err
                 handler {stderr, stdout} = 
                   size stderr >>= 
                     (\s -> if s > 0 then toString UTF8 stderr 
                                            >>= (\err -> errcb $ error err)
                                     else toString UTF8 stdout
                                            >>= (\res -> okcb res))
              in execFile cmd args ops handler)
