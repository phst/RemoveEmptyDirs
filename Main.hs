-- Written in 2014 by Philipp Stephani <p.stephani2@gmail.com>.
--
-- To the extent possible under law, the author has dedicated all copyright and
-- related and neighboring rights to this software to the public domain worldwide.
-- This software is distributed without any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication along with
-- this software.  If not, see http://creativecommons.org/publicdomain/zero/1.0/.

module Main (main) where

import System.Console.GetOpt (ArgDescr(NoArg), ArgOrder(Permute), OptDescr(Option), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import System.Log.Logger (Priority(CRITICAL, DEBUG, INFO))

import RemoveEmptyDirs (run, setLogLevel)

main :: IO ()
main = do
  args <- getArgs
  (opts, dirs) <- parseArgs args
  mapM_ applyOption opts
  run dirs

parseArgs :: [String] -> IO ([Priority], [FilePath])
parseArgs args = case getOpt Permute options args of
  (_, [], []) -> usageError "no directories given"
  (opts, dirs, []) -> return (opts, dirs)
  (_, _, errors) -> usageError $ concat errors

applyOption :: Priority -> IO ()
applyOption = setLogLevel

options :: [OptDescr Priority]
options = [
  Option "q" ["quiet"] (NoArg CRITICAL) "don't print error messages",
  Option "v" ["verbose"] (NoArg INFO) "print informational messages",
  Option "d" ["debug"] (NoArg DEBUG) "print debug messages"]

usageError :: String -> IO a
usageError message = do
  printUsage
  ioError $ userError message

printUsage :: IO ()
printUsage = do
  program <- getProgName
  let header = "Usage: " ++ program ++ " [options] directories"
  hPutStrLn stderr $ usageInfo header options
