module Main where

import qualified TextSearcher as TS
import Test.HUnit

data SearchTest = SearchTest {
  testName :: String,
  term :: String,
  context :: Int,
  expected :: [String]
} deriving (Show)

data FileTest = FileTest {
  filename :: String,
  searches :: [SearchTest]
} deriving (Show)

testSearch :: TS.SearchData -> SearchTest -> IO ()
testSearch searchData searchTest =
  let actual = TS.search searchData (term searchTest) (context searchTest)
  in assertEqual (testName searchTest) (expected searchTest) actual

testFile :: FileTest -> IO ()
testFile fileTest = do
  searchData <- TS.searchDataFromFile (filename fileTest)
  sequence_ $ fmap (testSearch searchData) (searches fileTest)

fileTests :: [FileTest]
fileTests = [
  FileTest "files/short_excerpt.txt"
    [
      SearchTest "testOneHitNoContext" "sketch" 0 ["sketch"],
      SearchTest "testMultipleHitsNoContext" "naturalists" 0 ["naturalists", "naturalists"],
      SearchTest "testBasicSearch" "naturalists" 0 [
        "great majority of naturalists believed that species",
        "authors.  Some few naturalists, on the other" ]

    ],
  FileTest "files/long_excerpt.txt"
    [
    ]
  ]

main :: IO ()
main = sequence_ $ fmap testFile fileTests
