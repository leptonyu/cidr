{-# LANGUAGE BinaryLiterals #-}
module Network.Cidr where

import           Data.Bits
import           Data.List       (intercalate)
import qualified Data.Map.Strict as HM
import qualified Data.Set        as HS
import           Data.Word
import           Text.Regex.PCRE

import Debug.Trace

type CIDR = (Word32, Word32)

fromPrefix :: String -> Maybe CIDR
fromPrefix ips = case ips =~ "(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)/(\\d+)" :: (String,String,String,[String]) of
  (_,_,_,[a,b,c,d,e]) ->
    let ip = shift (read a) 24 + shift (read b) 16 + shift (read c) 8 + read d
    in Just $ normalizePrefix (ip, read e)
  _ -> Nothing

normalizePrefix :: CIDR -> CIDR
normalizePrefix (_, 0) = (0, 0)
normalizePrefix (a,b) =
  let mk = foldl1 (.|.) $ take (fromIntegral b) $ iterate (`shift` 1) (shift 1 $ 32 - fromIntegral b)
  in (a .&. mk, b)

dualPrefix :: CIDR -> CIDR
dualPrefix (a,b) = let x = shift 1 (fromIntegral $ 32 - b) in normalizePrefix (if a .&. x == 0 then a + x else a - x, b)

succPrefix :: CIDR -> CIDR
succPrefix (a, b) = normalizePrefix (a, b - 1)

rangePrefix :: CIDR -> (Word32, Word32)
rangePrefix (p,l) = (p, p + (shift 1 (32 - fromIntegral l)))

toPrefix :: CIDR -> String
toPrefix (nid, plen) =
  let d = nid .&. 0b11111111
      c = (shiftR nid 8)  .&. 0b11111111
      b = (shiftR nid 16) .&. 0b11111111
      a = (shiftR nid 24) .&. 0b11111111
  in intercalate "." [show a, show b, show c, show d] ++ "/" ++ show plen

toBinaryPrefix :: CIDR -> String
toBinaryPrefix (i, x) = fmap (\a -> if shift 1 (32-a) .&. i == 0 then '0' else '1') [1..32] ++ "/" ++ show x


type SET = HM.Map Word32 Word32

merge :: CIDR -> SET -> SET
merge ci@(p,l)
  = g2 (dualPrefix ci)
  . HM.filterWithKey (go $ rangePrefix ci)
  where
    go (f,t) k v = k < f || (t /= 0 && k >= t)
    g2 di@(pd,ld) mx
      | HM.lookup pd mx == Just ld = merge (succPrefix ci) $ HM.delete pd mx
      | otherwise = g3 [1..fromIntegral l] mx
    g3 []     s = HM.insert p l s
    g3 (i:is) s =
      let (a,b) = normalizePrefix (p, i)
      in if HM.lookup a s == Just b then s else g3 is s

