module Beginner173 where

-- import Data.List
import Control.Monad (replicateM)

_a :: IO ()
_a = do
    a <- (readLn :: IO Int)
    let res = a `mod` 1000
    print (1000 - res)

_b :: IO ()
_b = do
    n <- readLn
    ss <- replicateM n getLine
    putStrLn $ "AC x " ++ show (count "AC" ss)
    putStrLn $ "WA x " ++ show (count "WA" ss)
    putStrLn $ "TLE x " ++ show (count "TLE" ss)
    putStrLn $ "RE x " ++ show (count "RE" ss)
        where 
            count :: Eq a => a -> [a] -> Int
            count _ [] = 0
            count y (x:xs) 
                | x == y = 1 + count y xs
                | otherwise = count y xs

_c :: IO ()
_c = do
    [h, w, k] <- map read . words <$> getLine
    xs <- replicateM h getLine 
    let selects = [(hs, ws) | hs <- subs [1..h], ws <- subs [1..w]]
    let res = map (validate xs) selects
    print (count k res)
        where
            validate xs (hs, ws) = 
                let rm1 = reduce 1 hs xs
                    rm2 = map (reduce 1 ws) rm1
                in count '#' $ concat rm2
            reduce _ _  [] = []
            reduce n hs (x:xs) = if n `elem` hs
                then reduce (n + 1) hs xs
                else x : reduce (n + 1) hs xs
            count :: Eq a => a -> [a] -> Int
            count _ [] = 0
            count y (x:xs) 
                | x == y = 1 + count y xs
                | otherwise = count y xs

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where yss = subs xs
    