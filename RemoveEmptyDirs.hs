-- Written in 2014 by Philipp Stephani <p.stephani2@gmail.com>.
--
-- To the extent possible under law, the author has dedicated all copyright and
-- related and neighboring rights to this software to the public domain worldwide.
-- This software is distributed without any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication along with
-- this software.  If not, see http://creativecommons.org/publicdomain/zero/1.0/.

import System.IO.Error (tryIOError)
import System.Console.GetOpt (ArgDescr(NoArg), ArgOrder(Permute), OptDescr(Option), getOpt, usageInfo)
import System.Directory (doesDirectoryExist, getDirectoryContents, removeFile)
import System.FilePath ((</>))
import System.Environment (getArgs, getProgName)
import System.Log.Logger (Priority(CRITICAL, DEBUG, INFO), errorM, setLevel, updateGlobalLogger)

loggerName :: String
loggerName = "RemoveEmptyDirs"

main :: IO ()
main = do
  args <- getArgs
  (opts, dirs) <- parseArgs args
  mapM_ applyOption opts
  mapM_ traverse dirs

parseArgs :: [String] -> IO ([Priority], [FilePath])
parseArgs args =
  let options = [
        Option "q" ["quiet"] (NoArg CRITICAL) "don't print error messages",
        Option "v" ["verbose"] (NoArg INFO) "print informational messages",
        Option "d" ["debug"] (NoArg DEBUG) "print debug messages"
        ]
  in case getOpt Permute options args of
    (opts, dirs, []) -> return (opts, dirs)
    (_, _, errors) -> do
      program <- getProgName
      let header = "Usage: " ++ program ++ " [options] directories"
      ioError $ userError $ concat errors ++ usageInfo header options

applyOption :: Priority -> IO ()
applyOption prio = updateGlobalLogger loggerName $ setLevel prio

traverse :: FilePath -> IO Entry
traverse dir = do
  let process "." = return Ignore
      process ".." = return Ignore
      process name@".DS_Store" = return $ JunkFile (dir </> name)
      process name = do
        let path = dir </> name
            processDir = do
              res <- tryIOError $ traverse path
              case res of
                Right r -> return r
                Left ex -> do
                  warn ex
                  return $ Directory path
            processFile = return $ PreciousFile path
        isDir <- doesDirectoryExist path
        if isDir then processDir else processFile
  contents <- getDirectoryContents dir >>= mapM process
  let ignore Ignore = True
      ignore _ = False
      junkFile (JunkFile _) = True
      junkFile _ = False
      entries = filter ignore contents
      returnDir = return $ Directory dir
      removeDir = do
        let remove (JunkFile f) = do
              res <- tryIOError $ removeFile f
              case res of
                Right _ -> return True
                Left ex -> do
                  warn ex
                  return False
            remove _ = error "remove applied to invalid entry"
        successful <- mapM remove entries
        return $ if and successful then Ignore else Directory dir
  if all junkFile entries then removeDir else returnDir

data Entry = Ignore
             | PreciousFile FilePath
             | JunkFile FilePath
             | Directory FilePath

warn :: IOError -> IO ()
warn ex = errorM loggerName $ show ex
