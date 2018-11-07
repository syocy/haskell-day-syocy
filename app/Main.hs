module Main where

import Lib
import Control.Parallel
import Control.Parallel.Strategies
import Control.Concurrent
import System.Mem
import System.Environment

xs :: [Int]
xs = [0..100000]

xxs :: [[Int]]
xxs = f xs []
  where
    f [] rs = rs
    f xs rs = let (h,t) = splitAt 100 xs in f t (h : rs)

ys :: [Double]
ys = [1..100]

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  let rrr = map sum xxs `using` parList rseq
  let ret = sum rrr
  print ret
-- main = do
--   [x, y] <- fmap (map readInt) getArgs
--   threadDelay 100
--   print $ mutualPow x y
