-- Copyright 2014 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main (main) where

import Control.Monad (when)
import Data.String.Utils (rstrip)
import Data.Version (showVersion)
import System.Console.GetOpt (ArgDescr(NoArg), ArgOrder(Permute), OptDescr(Option), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Log.Logger (Priority(CRITICAL, DEBUG, INFO))

import Paths_remove_empty_dirs (version)
import RemoveEmptyDirs (run, setLogLevel)

main :: IO ()
main = do
  args <- getArgs
  (opts, dirs) <- parseArgs args
  mapM_ applyOption opts
  when (null dirs) $ usageError "no directories given"
  run dirs

parseArgs :: [String] -> IO ([Action], [FilePath])
parseArgs args = case getOpt Permute options args of
  (opts, dirs, []) -> return (opts, dirs)
  (_, _, errors) -> usageError $ rstrip $ concat errors

data Action = PrintUsage
            | PrintVersion
            | SetLogLevel Priority

applyOption :: Action -> IO ()
applyOption PrintUsage = printUsage >> exitSuccess
applyOption PrintVersion = printVersion >> exitSuccess
applyOption (SetLogLevel prio) = setLogLevel prio

options :: [OptDescr Action]
options = [
  Option "d" ["debug"] (NoArg $ SetLogLevel DEBUG) "print debug messages",
  Option "h" ["help"] (NoArg PrintUsage) "print this usage information and exit",
  Option "q" ["quiet"] (NoArg $ SetLogLevel CRITICAL) "don't print error messages",
  Option "v" ["verbose"] (NoArg $ SetLogLevel INFO) "print informational messages",
  Option "V" ["version"] (NoArg PrintVersion) "print version number and exit"]

usageError :: String -> IO a
usageError message = do
  printUsage
  ioError $ userError message

printUsage :: IO ()
printUsage = do
  program <- getProgName
  let header = "Usage: " ++ program ++ " [options] directories"
  hPutStr stderr $ usageInfo header options

printVersion :: IO ()
printVersion = do
  program <- getProgName
  hPutStrLn stderr $ program ++ " " ++ showVersion version
