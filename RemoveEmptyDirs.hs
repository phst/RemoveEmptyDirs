-- Written in 2014 by Philipp Stephani <p.stephani2@gmail.com>.
--
-- To the extent possible under law, the author has dedicated all copyright and
-- related and neighboring rights to this software to the public domain worldwide.
-- This software is distributed without any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication along with
-- this software.  If not, see http://creativecommons.org/publicdomain/zero/1.0/.

module RemoveEmptyDirs (run, setLogLevel) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Directory (doesDirectoryExist, getDirectoryContents, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import System.Log.Logger (Priority, debugM, errorM, infoM, setLevel, updateGlobalLogger)

run :: [FilePath] -> IO ()
run = mapM_ traverse

traverse :: FilePath -> IO Entry
traverse dir = do
  debug $ "traversing directory " ++ dir
  let process "." = return Ignore
      process ".." = return Ignore
      process name@".DS_Store" = do
        let path = dir </> name
        debug $ "processing junk file " ++ path
        return $ JunkFile path
      process name = do
        let path = dir </> name
            processDir = do
              debug $ "processing directory " ++ path
              res <- tryIOError $ traverse path
              case res of
                Right r -> return r
                Left ex -> do
                  err $ show ex
                  return $ Directory path
            processFile = do
              debug $ "processing precious file " ++ path
              return $ PreciousFile path
        isDir <- doesDirectoryExist path
        if isDir then processDir else processFile
  contents <- getDirectoryContents dir >>= mapM process
  let ignore Ignore = True
      ignore _ = False
      junkFile (JunkFile _) = True
      junkFile _ = False
      entries = filter (not . ignore) contents
      removeJunk = do
        debug $ "removing junk files in directory " ++ dir
        let remove (JunkFile f) = do
              debug $ "removing junk file " ++ f
              res <- tryIOError $ removeFile f
              case res of
                Right _ -> do
                  info $ "removed junk file " ++ f
                  return True
                Left ex -> do
                  err $ show ex
                  return False
            remove _ = error "remove applied to invalid entry"
        and <$> mapM remove entries
      returnDir = do
        debug $ "directory " ++ dir ++ " is not empty"
        return $ Directory dir
      removeDir = do
        success <- removeJunk
        when success $ info $ "removed all junk files in directory " ++ dir
        let remove = do
              debug $ "removing empty directory " ++ dir
              res <- tryIOError $ removeDirectory dir
              case res of
                Right _ -> do
                  info $ "removed empty directory " ++ dir
                  return Ignore
                Left ex -> do
                  err $ show ex
                  returnDir
        if success then remove else returnDir
  if all junkFile entries then removeDir else returnDir

data Entry = Ignore
             | PreciousFile FilePath
             | JunkFile FilePath
             | Directory FilePath

err :: String -> IO ()
err = errorM loggerName

info :: String -> IO ()
info = infoM loggerName

debug :: String -> IO ()
debug = debugM loggerName

setLogLevel :: Priority -> IO ()
setLogLevel prio = updateGlobalLogger loggerName $ setLevel prio

loggerName :: String
loggerName = "RemoveEmptyDirs"
