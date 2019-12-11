module Main where

import           Data.Bits
import           Network.Cidr
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "randomTest" $ do
    it "random - from to"
      $ quickCheck
      $ \a b ->
          let x@(c,d) = normalizePrefix (a, mod b 31 + 1)
          in Just x == fromPrefix (toPrefix x)
    it "random - dual"
      $ quickCheck
      $ \a b ->
          let x@(c,d) = normalizePrefix (a, mod b 31 + 1)
              (p,q) = dualPrefix x
              delta = shift 1 (32- fromIntegral d)
          in q == d && if p < c then p + delta == c else c + delta == p
    it "random - succ"
      $ quickCheck
      $ \a b ->
          let x@(c,d)  = normalizePrefix (a, mod b 31 + 1)
          in succPrefix x == succPrefix (dualPrefix x)
