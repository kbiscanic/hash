module Language.Commands where

import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (chr)
import qualified Data.List             as L
import qualified Data.Map              as M
import           Language.Exec
import           Network
import           Numeric               (showHex)
import           System.Directory
import           System.IO

-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList [("echo", cmdEcho), ("mv", cmdMv), ("cp", cmdCp), ("rm", cmdRm),
  ("create", cmdCreate), ("rndir", cmdRnDir), ("cpdir", cmdCpDir), ("mkdir", cmdMkDir),
  ("rmdir", cmdRmDir), ("ls", cmdLs), ("pwd", cmdPwd), ("cd", cmdCd), ("cat", cmdCat),
  ("hexdump", cmdHexDump)]

cmdEcho :: Command
cmdEcho ss (ScriptState _ wd vars) = return $ ScriptState (concat ss) wd vars

cmdMv :: Command
cmdMv args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 2
  if pass then do
    isFile <- doesFileExist $ head args
    let targetDir = last args
    createDirectoryIfMissing True targetDir
    if isFile then mapM_ (\x -> copyFile x (targetDir ++ "/" ++ x)) (init args)
      else mapM_ (\x -> renameDirectory x (targetDir ++ "/" ++ x)) (init args)
    Control.Monad.when isFile $ mapM_ removeFile (init args)
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdCp :: Command
cmdCp args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 2
  if pass then do
    let targetDir = last args
    createDirectoryIfMissing True targetDir
    mapM_ (\x -> copyFile x (targetDir ++ "/" ++ x)) (init args)
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdRm :: Command
cmdRm args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    mapM_ removeFile args
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdCreate :: Command
cmdCreate args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    mapM_ (`openFile` WriteMode) args
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdRnDir :: Command
cmdRnDir args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 2
  if pass then do
    renameDirectory (head args) (last args)
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdCpDir :: Command
cmdCpDir args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 2
  if pass then do
    let targetDir = last args
    createDirectoryIfMissing True targetDir
    mapM_ (\x -> createDirectoryIfMissing False (targetDir ++ "/" ++ x)) (init args)
    mapM_ (\x -> copyFiles x (targetDir ++ "/" ++ x)) (init args)
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles from to = do
  files <- getDirectoryContents from
  mapM_ (\x -> copyFile (from ++ "/" ++ x) (to ++ "/" ++ x)) (init $ init files)

cmdMkDir :: Command
cmdMkDir args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    let targetDir = head args
    createDirectoryIfMissing True targetDir
    return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdRmDir :: Command
cmdRmDir args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    let targetDir = head args
    files <- getDirectoryContents targetDir
    if length files > 2 then return $ scst{output = "Failure - directory not empty!"} else do
      removeDirectory targetDir
      return $ scst{output = "Success!"}
  else return $ scst{output = "Failure!"}

cmdLs :: Command
cmdLs args scst@(ScriptState _ wd vars) = do
  let dir = if null args then wd else head args
  files <- getDirectoryContents dir
  return $ scst{output = unlines files}

cmdPwd :: Command
cmdPwd args scst@(ScriptState _ wd vars) = return $ scst{output = wd}

cmdCd :: Command
cmdCd [] scst@(ScriptState _ _ vars) = do
  dir <- getHomeDirectory
  setCurrentDirectory dir
  return $ scst{output = "", wd = dir}
cmdCd args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    setCurrentDirectory $ head args
    dir <- getCurrentDirectory
    return $ scst{output = "", wd = dir}
  else return $ scst{output = "Failure!"}

cmdCat :: Command
cmdCat args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    cat <- readFile $ head args
    return $ scst{output = cat}
  else return $ scst{output = "Failure!"}

cmdHexDump :: Command
cmdHexDump args scst@(ScriptState _ wd vars) = do
  pass <- checkArgs args 1
  if pass then do
    bs <- B.readFile $ head args
    let hex = bToHex bs
    return $ scst{output = hex}
  else return $ scst{output = "Failure!"}

bToHex :: B.ByteString -> String
bToHex = L.intercalate "  "
          . map sshowHex
          . B.unpack

sshowHex :: (Show a, Integral a) => a -> String
sshowHex n = showHex n ""

checkArgs :: [String] -> Int -> IO Bool
checkArgs xs n =
  if length xs < n then do
    putStrLn $ "Must supply " ++ show n ++ " argument(s)!"
    return False
  else return True
