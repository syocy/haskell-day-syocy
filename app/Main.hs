module Main where

import Lib
import Control.Parallel
import Control.Parallel.Strategies
import GHC.Compact

xs :: [Int]
xs = [0..100000]

xxs :: [[Int]]
xxs = f xs []
  where
    f [] rs = rs
    f xs rs = let (h,t) = splitAt 100 xs in f t (h : rs)

main :: IO ()
main = do
  let rrr = map sum xxs `using` parList rseq
  let ret = sum rrr
  print ret
