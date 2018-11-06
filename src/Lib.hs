module Lib where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- |
-- >>> plus 1 2
-- 3
plus :: Int -> Int -> Int
plus = (+)

-- |
-- >>> helloworld1
-- hello
-- world
helloworld1 :: IO ()
helloworld1 = do
  t1 <- async $ do   -- 軽量スレッド t1 を開始
    threadDelay 10   ---- 10μs スリープ
    putStrLn "world" ---- "world" を出力
  putStrLn "hello"   -- "hello" を出力
  wait t1            -- スレッド t1 の終了を待つ






-- |
-- >>> helloworld
-- Hello, World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
helloworld :: IO ()
helloworld = do
  ts <- replicateM 100 $ async $ do -- スレッド100個作る
          threadDelay 100           ---- 100μsスリープ
          putStr "!"                ---- "!" を出力
  putStr "Hello, World"             -- "Hello, World"
  forM_ ts wait                     -- スレッド終了待ち
  putStrLn ""









-- |
-- >>> atomicMap
-- fromList [("someKey",1000)]
atomicMap :: IO ()
atomicMap = do
  amap <- newTVarIO (Map.empty :: Map String Int)
  ts <- replicateM 1000 $ async $ do
          atomically $ do
            modifyTVar' amap $ Map.alter inc "someKey"
  forM_ ts wait
  print =<< atomically (readTVar amap)
    where
      inc Nothing = Just 1
      inc (Just x) = Just (x+1)
