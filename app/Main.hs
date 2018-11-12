module Main where

import Lib
import Control.Parallel
import Control.Parallel.Strategies
import Control.Concurrent
import System.Mem
import System.Environment

xs :: [Int]
xs = [0..100000]

chunkedXs :: [[Int]]
chunkedXs = f xs []
  where
    f [] rs = rs
    f xs rs = let (h,t) = splitAt 100 xs in f t (h : rs)

main :: IO ()
main = do
  let tmp = map sum chunkedXs `using` parList rseq
  let ret = sum tmp
  print ret
