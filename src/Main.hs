module Main where

import qualified TextSearcher as TS
import System.Environment

main :: IO ()
main =
  let sampleText = "34 fff ggg 34 fff 45 45 fff"
      searchData = TS.mkSearchData sampleText
      result = TS.search searchData "fff" 2
  in do
    putStrLn $ (show searchData)
    putStrLn $ (show result)
