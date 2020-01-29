module Main where

import Data.Maybe (isNothing)
import TextSearcher
import System.Environment
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= mainArgs

mainArgs :: [String] -> IO ()
mainArgs args
  | length args /= 1                               = usage
  | otherwise                                      =
      let filename = head args in
      putStrLn $ (show (mkSearchData "34 fff ggg 34 fff 45 45"))

usage :: IO ()
usage = hPutStrLn stderr ("usage: text-searcher <filename>")
