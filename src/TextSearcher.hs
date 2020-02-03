module TextSearcher (SearchData, searchDataFromFile, searchDataFromString, search) where

import Control.Applicative (liftA2)
import Data.Char (toUpper)
import Data.List (group, sort)
import System.IO
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Text.Regex as R

type TermLookup = M.HashMap String [Int]

data SearchData = SearchData {
  document :: String,
  leftBounds :: V.Vector Int,
  rightBounds :: V.Vector Int,
  termLookup :: TermLookup
} deriving (Show)

searchDataFromFile :: String -> IO SearchData
searchDataFromFile filename = do
  contents <- readFile filename
  return $ searchDataFromString contents

searchDataFromString :: String -> SearchData
searchDataFromString document =
  let rows = searchDataRows document 0
  in toSearchData document rows

data SearchDataRow = SearchDataRow {
  leftBound :: Int,
  rightBound :: Int,
  searchTerm :: String
} deriving (Show)

searchDataRows :: String -> Int -> [SearchDataRow]
searchDataRows input index =
  let nextMatch = R.matchRegexAll tokenizer input
  in case nextMatch of
    Just (pre, match, post, _) ->
      let
        matchIndex = index + length pre
        postIndex = matchIndex + length match
        searchTerm = toSearchTerm match
        row = SearchDataRow matchIndex postIndex searchTerm
      in row : searchDataRows post postIndex
    Nothing -> []

tokenizer :: R.Regex
tokenizer = R.mkRegex "[a-zA-Z0-9']+"

toSearchTerm :: String -> String
toSearchTerm word = fmap toUpper word

toSearchData :: String -> [SearchDataRow] -> SearchData
toSearchData document rows =
  let leftBounds = V.fromList $ leftBound <$> rows
      rightBounds = V.fromList $ rightBound <$> rows
      termLookup = toTermLookup $ searchTerm <$> rows
  in SearchData document leftBounds rightBounds termLookup

toTermLookup :: [String] -> TermLookup
toTermLookup terms = snd $ foldl addTerm (0, M.empty) terms

addTerm :: (Int, TermLookup) -> String -> (Int, TermLookup)
addTerm (index, lookup) term =
  let f new old = old ++ new
      newLookup = M.insertWith f term [index] lookup
  in (index + 1, newLookup)

search :: SearchData -> String -> Int -> [String]
search searchData term context =
  let searchTerm = toSearchTerm term
      lookup = termLookup searchData
      termIndices = M.lookupDefault [] searchTerm lookup
  in searchResult searchData context <$> termIndices

searchResult :: SearchData -> Int -> Int -> String
searchResult searchData context termIndex =
  let
    doc = document searchData
    lefts = leftBounds searchData
    startIndex = termIndex - context
    startPos = maybe 0 id $ lefts V.!? startIndex
    rights = rightBounds searchData
    endIndex = termIndex + context
    endPos = maybe (length doc) id $ rights V.!? endIndex
  in drop startPos $ take endPos doc
