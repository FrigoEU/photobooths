module App.Exec (exec) where

import Prelude (Unit, otherwise, (<>), ($), (==), (<<<), bind, show, (>>=), unit, pure)

import Data.String (stripSuffix)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right), either)
import Data.StrMap (StrMap)

import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, makeAff)

import Node.ChildProcess as CP
import Node.Process as Process
import Node.Platform (Platform(Win32))

-- | copied from: pulp purescript rewrite:
-- | Start a child process asynchronously, with the given command line
-- | arguments and environment, and wait for it to exit.
-- | On a non-zero exit code, throw an error.
--
-- | If the executable was not found and we are on Windows, retry with ".cmd"
-- | appended.
-- |
-- | Stdout, stdin, and stderr of the child process are shared with the pulp
-- | process (that is, data on stdin from pulp is relayed to the child process,
-- | and any stdout and stderr from the child process are relayed back out by
-- | pulp, which usually means they will immediately appear in the terminal).
exec :: forall eff. String -> Array String -> Maybe (StrMap String) 
                    -> Aff (cp :: CP.CHILD_PROCESS, err :: EXCEPTION | eff) Unit
exec cmd args env = do
  child <- liftEff $ CP.spawn cmd args (def { env = env
                                            , stdio = CP.inherit })
  wait child >>= either (handleErrors cmd retry) onExit

  where
  def = CP.defaultSpawnOptions

  onExit exit =
    case exit of
      CP.Normally 0 -> pure unit
      _             -> throwError $ error $ "Subcommand terminated " <> showExit exit

  retry newCmd = exec newCmd args env
  
showExit :: CP.Exit -> String
showExit (CP.Normally x) = "with exit code " <> show x
showExit (CP.BySignal sig) = "as a result of receiving " <> show sig

-- | A slightly weird combination of `onError` and `onExit` into one.
wait :: forall eff. CP.ChildProcess ->  Aff (cp :: CP.CHILD_PROCESS | eff) (Either CP.Error CP.Exit)
wait child = makeAff \_ win -> do
  CP.onExit child (win <<< Right)
  CP.onError child (win <<< Left)
  
handleErrors :: forall a eff. String -> (String -> Aff (err :: EXCEPTION | eff) a) -> CP.Error 
                              -> Aff (err :: EXCEPTION | eff) a
handleErrors cmd retry err
  | err.code == "ENOENT" = do
     -- On windows, if the executable wasn't found, try adding .cmd
     if Process.platform == Win32
       then case stripSuffix ".cmd" cmd of
              Nothing      -> retry (cmd <> ".cmd")
              Just bareCmd -> throwError $ error $
                 "`" <> bareCmd <> "` executable not found. (nor `" <> cmd <> "`)"
       else
         throwError $ error $
           "`" <> cmd <> "` executable not found."
  | otherwise =
     throwError (CP.toStandardError err) 
