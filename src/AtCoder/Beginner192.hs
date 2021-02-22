module Beginner192 where

-- import Control.Monad (forM_, replicateM)
-- import Control.Monad.ST.Strict ( runST )
-- import Data.STRef (readSTRef)
-- import Data.STRef.Strict ( modifySTRef, newSTRef )

import Data.List (sort)

_a :: IO ()
_a = do
    x <- (read :: String -> Int) <$> getLine
    print $ 100 - (x `mod` 100)

_b :: IO ()
_b = do
    s <- getLine
    putStrLn $ if convert s then "Yes" else "No"
        where
            convert (x : y : zs) = isLower x && isCapital y && convert zs
            convert [x] = isLower x
            convert [] = True
            isLower x = 'a' <= x && x <= 'z'
            isCapital x = 'A' <= x && x <= 'Z'

_c :: IO ()
_c = do
    [n, k] <- map (read :: String -> Int) . words <$> getLine
    print $ (iterate f n) !! k
        where 
            g1 :: Int -> Int
            g1 = read . reverse . sort . show
            g2 :: Int -> Int
            g2 = read . sort . show
            f :: Int -> Int
            f x = g1 x - g2 x