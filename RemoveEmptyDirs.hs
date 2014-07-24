-- Written in 2014 by Philipp Stephani <p.stephani2@gmail.com>.
--
-- To the extent possible under law, the author has dedicated all copyright and
-- related and neighboring rights to this software to the public domain worldwide.
-- This software is distributed without any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication along with
-- this software.  If not, see http://creativecommons.org/publicdomain/zero/1.0/.

import System.IO.Error (tryIOError)
import System.Directory (doesDirectoryExist, getDirectoryContents, removeFile)
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Log.Logger (errorM)

main :: IO ()
main = do
  dirs <- getArgs
  mapM_ traverse dirs

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
warn ex = errorM "RemoveEmptyDirs" $ show ex
