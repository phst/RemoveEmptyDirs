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
import Test.Framework (Test, TestName, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import qualified RemoveEmptyDirs

main :: IO ()
main = defaultMain cases

type FixtureTest a = ReaderT a IO ()
type FixtureWrapper a = (a -> Assertion) -> Assertion

runFixtureTest :: FixtureWrapper a -> FixtureTest a -> IO ()
runFixtureTest wrap = wrap . runReaderT

fixtureTestCase :: FixtureWrapper a -> TestName -> FixtureTest a -> Test
fixtureTestCase wrap name = testCase name . runFixtureTest wrap

newtype TempDirFixture = TempDirFixture FilePath
type TempDirTest = FixtureTest TempDirFixture

inTempDir :: TestName -> FixtureWrapper TempDirFixture
inTempDir name fn = withTemporaryDirectory ("HUnit test: " ++ name ++ ". XXXXXX") (fn . TempDirFixture)

tempDirCase :: TestName -> TempDirTest -> Test
tempDirCase = inTempDir >>= fixtureTestCase

data Directory = Directory {
  dirName :: String,
  dirFiles :: [String]
  } deriving (Show, Eq, Ord)

prepare :: [Directory] -> TempDirTest
prepare dirs = do
  f <- ask
  lift $ prepareIn f dirs

prepareIn :: TempDirFixture -> [Directory] -> IO ()
prepareIn (TempDirFixture root) dirs = do
  RemoveEmptyDirs.setLogLevel DEBUG
  forM_ dirs $ \(Directory dir files) -> do
    let path = root </> dir
    createDirectoryIfMissing True path
    forM_ files $ \f -> writeFile (path </> f) "test"

run :: [FilePath] -> TempDirTest
run dirs = do
  f <- ask
  lift $ runIn f dirs

runIn :: TempDirFixture -> [FilePath] -> IO ()
runIn (TempDirFixture root) dirs = RemoveEmptyDirs.run $ map (root </>) dirs

assert :: [Directory] -> TempDirTest
assert expected = do
  f <- ask
  lift $ assertIn f expected

assertIn :: TempDirFixture -> [Directory] -> Assertion
assertIn (TempDirFixture root) expected = do
  entries <- find root
  let conv "." _ = Nothing
      conv p s | isDirectory s = Just $ Directory p []
               | otherwise = let dir = takeDirectory p
                             in Just $ Directory dir [makeRelative dir p]
      conv' = uncurry $ conv . makeRelative root
      merge dirs = Directory (dirName $ head dirs) (sort $ concatMap dirFiles dirs)
      actual = map merge $ groupBy ((==) `on` dirName) $ sort $ mapMaybe conv' entries
  assertEqual "directory structure after run incorrect" expected actual

assertThrows :: TempDirTest -> TempDirTest
assertThrows fn = do
  f <- ask
  mapReaderT (assertThrowsIn f) fn

assertThrowsIn :: TempDirFixture -> IO a -> Assertion
assertThrowsIn _ fn = do
  res <- tryIOError fn
  case res of
    Left _ -> return ()
    Right _ -> assertFailure "expected IOError"

cases :: [Test]
cases = [
  tempDirCase "non-existing directory" nonExistingDir,
  tempDirCase "nothing to do" nothing,
  tempDirCase "empty directory" emptyDir,
  tempDirCase "only junk files" onlyJunk,
  tempDirCase "junk and precious files" junkAndPrecious,
  tempDirCase "subdirectory" subdirectory,
  tempDirCase "existing and non-existing directories" existingAndNonExisting,
  tempDirCase "complex" complex
  ]

nonExistingDir :: TempDirTest
nonExistingDir = do
  prepare []
  assertThrows $ run ["foo"]
  assert []

nothing :: TempDirTest
nothing = do
  prepare []
  run []
  assert []

emptyDir :: TempDirTest
emptyDir = do
  prepare [Directory "foo" []]
  run ["foo"]
  assert []

onlyJunk :: TempDirTest
onlyJunk = do
  prepare [Directory "foo" [".DS_Store"]]
  run ["foo"]
  assert []

junkAndPrecious :: TempDirTest
junkAndPrecious = do
  prepare [Directory "foo" [".DS_Store", "precious"]]
  run ["foo"]
  assert [Directory "foo" [".DS_Store", "precious"]]

subdirectory :: TempDirTest
subdirectory = do
  prepare [Directory "foo" [], Directory "foo/bar" ["precious"]]
  run ["foo"]
  assert [Directory "foo" [], Directory "foo/bar" ["precious"]]

existingAndNonExisting :: TempDirTest
existingAndNonExisting = do
  prepare [Directory "foo" []]
  assertThrows $ run ["bar", "foo"]
  assert [Directory "foo" []]

complex :: TempDirTest
complex = do
  prepare [Directory "foo" [".DS_Store"],
           Directory "foo/bar" ["precious"],
           Directory "foo/bar/baz" [".DS_Store"],
           Directory "qux" []]
  run ["foo", "qux"]
  assert [Directory "foo" [".DS_Store"],
          Directory "foo/bar" ["precious"]]
