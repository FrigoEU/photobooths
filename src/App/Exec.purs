module App.Exec (simpleExec, simpleExecStr) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.StrMap (StrMap)
import Data.String (joinWith)
import Node.Buffer (BUFFER, toString)
import Node.ChildProcess (exec, CHILD_PROCESS, execFile)
import Node.Encoding (Encoding(UTF8))
import Prelude (show, ($), bind)

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
         handler {error: err, stderr, stdout} = do
           stdErrStr <- toString UTF8 stderr 
           stdOutStr <- toString UTF8 stdout 
           maybe 
             (okcb $ joinWith " " ["Success:", cmd, show args, "Stdout:", stdOutStr, "StdErr: ", stdErrStr])
             (\e -> errcb $ error $ joinWith " " ["Error:", cmd, show args, "Stdout:", stdOutStr, "StdErr: ", stdErrStr, "Error:", show e])
             err
      in execFile cmd args ops handler)

simpleExecStr :: forall eff. 
                 String 
                 -> Maybe String
                 -> Maybe (StrMap String) 
                 -> Aff (cp :: CHILD_PROCESS, err :: EXCEPTION, buffer :: BUFFER | eff) String
simpleExecStr cmd cwd env =
  makeAff (\errcb okcb -> 
     let ops = { cwd: cwd
               , env: env
               , timeout: Just 5000.0
               , maxBuffer: Nothing
               , killSignal: Nothing
               , uid: Nothing
               , gid: Nothing }
         handler {error: err, stderr, stdout} = do
           stdErrStr <- toString UTF8 stderr 
           stdOutStr <- toString UTF8 stdout 
           maybe 
             (okcb $ joinWith " " ["Success:", cmd, "Stdout:", stdOutStr, "StdErr: ", stdErrStr])
             (\e -> errcb $ error $ joinWith " " ["Error:", cmd, "Stdout:", stdOutStr, "StdErr: ", stdErrStr, "Error:", show e])
             err
      in exec cmd ops handler)
