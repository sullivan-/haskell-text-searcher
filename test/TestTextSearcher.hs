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
      SearchTest "testBasicSearch" "naturalists" 3 [
        "great majority of naturalists believed that species",
        "authors.  Some few naturalists, on the other" ],
      SearchTest "testBasicMoreContext" "naturalists" 6 [
        "Until recently the great majority of naturalists believed that species were immutable productions",
        "maintained by many authors.  Some few naturalists, on the other hand, have believed" ],
      SearchTest "testCaseInsensitiveSearch 1" "species" 4 [
        "on the Origin of Species.  Until recently the great",
        "of naturalists believed that species were immutable productions, and",
        "hand, have believed that species undergo modification, and that" ],
      SearchTest "testCaseInsensitiveSearch 2" "SPECIES" 4 [
        "on the Origin of Species.  Until recently the great",
        "of naturalists believed that species were immutable productions, and",
        "hand, have believed that species undergo modification, and that" ],
      SearchTest "testCaseInsensitiveSearch 3" "SpEcIeS" 4 [
        "on the Origin of Species.  Until recently the great",
        "of naturalists believed that species were immutable productions, and",
        "hand, have believed that species undergo modification, and that" ],
      SearchTest "testNearBeginning" "here" 4 [
        "I will here give a brief sketch" ],
      SearchTest "testNearEnd" "existing" 3 [
        "and that the existing forms of life",
          "generation of pre existing forms." ],
      SearchTest "testOverlappingHits" "that" 3 [
        "of naturalists believed that species were immutable",
        "hand, have believed that species undergo modification",
        "undergo modification, and that the existing forms" ]
    ],
  FileTest "files/long_excerpt.txt"
    [
      SearchTest "testApostropheQuery" "animal's" 4 [
        "not indeed to the animal's or plant's own good",
        "habitually speak of an animal's organisation as\r\nsomething plastic" ],
      SearchTest "testNumericQuery" "1844" 2 [
        "enlarged in 1844 into a",
        "sketch of 1844--honoured me" ],
      SearchTest "testMixedQuery" "xxxxx10x" 3 [
        "date first edition [xxxxx10x.xxx] please check" ],
      SearchTest "testNoHits" "slejrlskejrlkajlsklejrlksjekl" 3 []
    ]
  ]

main :: IO ()
main = sequence_ $ fmap testFile fileTests
