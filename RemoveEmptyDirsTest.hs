-- Written in 2014 by Philipp Stephani <p.stephani2@gmail.com>.
--
-- To the extent possible under law, the author has dedicated all copyright and
-- related and neighboring rights to this software to the public domain worldwide.
-- This software is distributed without any warranty.
--
-- You should have received a copy of the CC0 Public Domain Dedication along with
-- this software.  If not, see http://creativecommons.org/publicdomain/zero/1.0/.

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List (groupBy, sort)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), makeRelative, takeDirectory)
import System.IO.Error (tryIOError)
import System.Log.Logger (Priority(DEBUG))
import System.Posix (isDirectory)
import System.Unix.Directory (find, withTemporaryDirectory)
import Test.Framework (defaultMain)
import qualified Test.Framework as Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import qualified RemoveEmptyDirs

main :: IO ()
main = defaultMain cases

cases :: [Framework.Test]
cases = [
  makeCase "non-existing directory" nonExistingDir,
  makeCase "nothing to do" nothing,
  makeCase "empty directory" emptyDir
  ]

newtype Fixture = Fixture FilePath
type Test = ReaderT Fixture IO ()

makeCase :: String -> Test -> Framework.Test
makeCase name test = testCase name $ withTemporaryDirectory "HUnitXXXXXX" $ runReaderT test . Fixture

data Directory = Directory {
  dirName :: String,
  dirFiles :: [String]
  } deriving (Show, Eq, Ord)

prepare :: [Directory] -> Test
prepare dirs = do
  f <- ask
  lift $ prepareIn f dirs

prepareIn :: Fixture -> [Directory] -> IO ()
prepareIn (Fixture root) dirs = do
  RemoveEmptyDirs.setLogLevel DEBUG
  forM_ dirs $ \(Directory dir files) -> do
    createDirectoryIfMissing True (root </> dir)
    forM_ files $ \f -> writeFile (dir </> f) "test"

run :: [FilePath] -> Test
run dirs = do
  f <- ask
  lift $ runIn f dirs

runIn :: Fixture -> [FilePath] -> IO ()
runIn (Fixture root) dirs = RemoveEmptyDirs.run $ map (root </>) dirs

assert :: [Directory] -> Test
assert expected = do
  f <- ask
  lift $ assertIn f expected

assertIn :: Fixture -> [Directory] -> Assertion
assertIn (Fixture root) expected = do
  entries <- find root
  let conv "." _ = Nothing
      conv p s | isDirectory s = Just $ Directory p []
               | otherwise = let dir = takeDirectory p
                             in Just $ Directory dir [makeRelative dir p]
      conv' = uncurry $ conv . makeRelative root
      merge dirs = Directory (dirName $ head dirs) (sort $ concatMap dirFiles dirs)
      actual = map merge $ groupBy ((==) `on` dirName) $ sort $ mapMaybe conv' entries
  assertEqual "directory structure after run incorrect" expected actual

assertThrows :: Test -> Test
assertThrows fn = do
  f <- ask
  mapReaderT (assertThrowsIn f) fn

assertThrowsIn :: Fixture -> IO a -> Assertion
assertThrowsIn _ fn = do
  res <- tryIOError fn
  case res of
    Left _ -> return ()
    Right _ -> assertFailure "expected IOError"

nonExistingDir :: Test
nonExistingDir = do
  prepare []
  assertThrows $ run ["foo"]

nothing :: Test
nothing = do
  prepare []
  run []
  assert []

emptyDir :: Test
emptyDir = do
  prepare [Directory "foo" []]
  run ["foo"]
  assert []
