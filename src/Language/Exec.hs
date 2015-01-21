module Language.Exec where

import qualified Data.Map             as M
import           Data.Maybe
import           Language.Expressions

-- A model of a command which is waiting for arguments and a state to run
type Command = [String] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String String
-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command
-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output   :: String
                               , wd       :: FilePath
                               , vartable :: VarTable
                               } deriving Show
-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command’s execution.
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram cmds (Left wd') es
  = runHashProgram cmds
      (Right ScriptState{output = "", wd = wd', vartable = M.empty}) es
runHashProgram cmds (Right scrState) (e:es) = do
  eval <- runTopLevel cmds scrState e
  runHashProgram cmds (Right eval) es
runHashProgram _ (Right scrState) _ = return scrState

-- Calculates the result of a top-level command execution
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel cmds scrState (TLCmd cmd) = runCmd cmds scrState cmd
runTopLevel cmds scrState e = return ScriptState{output = "", wd = "", vartable = M.empty}

-- The rest of the module should consist of similar functions, calling each
-- other so that each expression is parsed by a lower-level function and the
-- result can be used in a higher-level function. The Command table and state
-- are passed around as necessary to evaluate commands, assignments and
-- variable substitution. A better way to pass around variables would be to
-- use the State monad or even the StateT monad transformer to wrap IO into it.

lookItUp :: [String] -> VarTable -> [String] -> [String]
lookItUp [] _ sol = reverse sol
lookItUp (x:xs) vt sol = case M.lookup x vt of
  Just s -> lookItUp xs vt (s:sol)
  Nothing -> lookItUp xs vt (x:sol)

runCmd :: CommandTable -> ScriptState -> Cmd -> IO ScriptState
runCmd cmds scrState cmd@(Cmd{}) = do
  state <- case M.lookup (exToStr $ name cmd) cmds of
    Just cmdF -> do
      let ar = map exToStr $ args cmd
      let ar' = lookItUp ar (vartable scrState) []
      s <- cmdF ar' scrState
      if isNothing $ outDir cmd then putStrLn $ output s else
          (if append cmd then appendFile else writeFile)
            (exToStr $ fromJust $ outDir cmd)
            $ output s ++ "\n"
      return s
    Nothing -> do
      putStrLn $ "No command named: " ++ exToStr (name cmd)
      return scrState
  return state
  
runCmd cmds scrState (Assign var val) = case M.lookup (exToStr val) (vartable scrState) of
    Just s -> return scrState{vartable = M.insert (exToStr var) s (vartable scrState)}
    Nothing -> return scrState{vartable = M.insert (exToStr var) (exToStr val) (vartable scrState)}
