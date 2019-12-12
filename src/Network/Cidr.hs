{-# LANGUAGE BinaryLiterals #-}
module Network.Cidr where

import           Data.Bits
import           Data.List       (foldl', intercalate)
import qualified Data.Map.Strict as HM
import           Data.Word
import           Text.Regex.PCRE

import           Debug.Trace

type CIDR = (Word32, Word32)

type RANGE = (Word32, Word32)

regexIp :: String
regexIp = "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)"

regexIpBlock :: String
regexIpBlock = regexIp ++ "/(\\d+)"

fromPrefix :: String -> Maybe CIDR
fromPrefix ips = case ips =~ regexIpBlock :: (String,String,String,[String]) of
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

toRange :: CIDR -> RANGE
toRange (p, 0) = (p, maxBound)
toRange (p, l) = (p, p + (shift 1 (32 - fromIntegral l) - 1))

isLocal :: CIDR -> Bool
isLocal = go . toRange
  where
    go (f, t) = (f >= 167772160 && t <= 184549375)
      || (f >= 2886729728 && t <= 2887778303)
      || (f >= 3232235520 && t <= 3232301055)


fromRange :: RANGE -> Maybe CIDR
fromRange (f, t)
  | t - f == maxBound = Just (0, 0)
  | f > t  || popCount (t - f + 1) /= 1  = Nothing
  | otherwise = Just (f, fromIntegral $ countLeadingZeros (t - f))

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
  . HM.filterWithKey (go $ toRange ci)
  where
    go (f,t) k v = let (a,b) = toRange (k,v) in not (f <= a && b <= t)
    g2 di@(pd,ld) mx
      | HM.lookup pd mx == Just ld = merge (succPrefix ci) $ HM.delete pd mx
      | otherwise = g3 (succPrefix ci) mx
    g3 cn@(_,0) m3 = HM.insert p l m3
    g3 cn@(a,b) m3 = case HM.lookup a m3 of
      Just v -> if v <= b then m3 else HM.insert p l m3
      _      -> g3 (succPrefix cn) m3

reducePrefix :: (String -> Maybe CIDR) -> String -> String
reducePrefix f = unlines . map toPrefix . HM.toList . foldl' go HM.empty . lines
  where
    go ma s = case f s of
      Just a -> merge (normalizePrefix a) ma
      _      -> ma

type CountryCode = String

loadCsv :: String -> Maybe (CIDR, CountryCode)
loadCsv s = case s =~ "\"(\\d+)\",\"(\\d+)\",\"([-a-zA-Z]+)\",\"(.+?)\"" :: (String,String,String,[String]) of
  (_,_,_,[f,t,c,_]) -> case fromRange (read f, read t) of
    Just ip -> Just (ip, c)
    _       -> Nothing
  _ -> Nothing

loadCsvByCountry :: String -> String -> Maybe CIDR
loadCsvByCountry cc s = case loadCsv s of
  Just (ip, c) -> if c == cc then Just ip else Nothing
  _            -> fromPrefix s


