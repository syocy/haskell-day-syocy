module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- |
-- >>> plus 1 2
-- 3
plus :: Int -> Int -> Int
plus = (+)
