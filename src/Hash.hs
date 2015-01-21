-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
module Hash (runScript, runInteractive) where

import qualified Data.Map           as M
import           Language.Commands
import           Language.Exec
import           Parsing.HashParser
import           System.Directory
import           System.IO

-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript file = do
  wd' <- getCurrentDirectory
  scrState <- initHashRc wd'
  script <- readFile file
  case parseAll script of
    Left str -> putStrLn "Syntax error in file!"
    Right es -> do
      runHashProgram commands (Right scrState) es
      putStrLn "Execution finished"
  return ()

runScr :: FilePath -> ScriptState -> IO ScriptState
runScr fName scrState = do
  script <- readFile fName
  case parseAll script of
    Left str -> do
      putStrLn "Syntax error in file!"
      return scrState
    Right es -> runHashProgram commands (Right scrState) es

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  wd' <- getCurrentDirectory
  scrState <- initHashRc wd'
  runInteractive' scrState
  where runInteractive' :: ScriptState -> IO ()
        runInteractive' ss = do
          putStr $ wd ss
          putStr ">"
          line <- getLine
          case parseAll line of
            Left str -> do
              putStrLn $ "Error: " ++ str
              runInteractive' ss
            Right es -> do
              state <- runHashProgram commands (Right ss) es
              runInteractive' state

initHashRc :: FilePath -> IO ScriptState
initHashRc wd' = do
  homeDir <- getHomeDirectory
  let fName = homeDir ++ "\\.hashrc.hash"
  isFile <- doesFileExist fName
  if isFile then runScr fName ScriptState{output = "", wd = wd', vartable = M.empty}
    else return ScriptState{output = "", wd = wd', vartable = M.empty}
