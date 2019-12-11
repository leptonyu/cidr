module Main where

import           Data.List       (foldl')
import qualified Data.Map.Strict as HM
import           Network.Cidr

main :: IO ()
main = interact $ unlines . map toPrefix . HM.toList . foldl' go HM.empty . lines
  where
    go m s = case fromPrefix s of
      Just a -> merge (normalizePrefix a) m
      _      -> m
