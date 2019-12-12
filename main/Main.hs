module Main where

import           Network.Cidr
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  interact (reducePrefix $ loadCsvByCountry $ go args)
  where
    go []    = "US"
    go (a:_) = a
