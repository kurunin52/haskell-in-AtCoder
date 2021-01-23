module Beginner172 where

-- import System.IO
-- import Data.List

__a :: IO ()
__a = do
    a <- (readLn :: IO Int)
    print (a + a ^ (2 :: Int) + a ^ (3 :: Int))

__b :: IO ()
__b = do
    s <- getLine 
    t <- getLine
    print (sum $ [if x == y then (0 :: Int) else (1 :: Int) | (x, y) <- zip s t])

__c :: IO ()
__c = do
    [_, _, k] <- map (read :: String -> Int) . words <$> getLine
    as <- map read . words <$> getLine
    bs <- map read . words <$> getLine
    let as' = takeWhile (\x -> snd x <= k) [(n, sum (take n as)) | n <- [0..length as]]
        ads = [k - n | (_ , n) <- as']
        bs' = [last $ takeWhile (\b -> sum (take b bs) <= x) [0 .. length bs] | x <- ads]
        tmp = zipWith (\(a, _) b -> a + b) as' bs'
        res = maximum tmp
    print res
    -- putStrLn $ show $ eval as bs k
        -- where
        --     eval _      _      0 = 0
        --     eval []     []     _ = 0
        --     eval (x:xs) []     n = if x <= n then 1 + eval xs [] (n - x) else 0
        --     eval []     (y:ys) n = if y <= n then 1 + eval [] ys (n - y) else 0
        --     eval (x:xs) (y:ys) n
        --         | x <= y    = if x <= n then 1 + eval xs     (y:ys) (n - x) else 0
        --         | otherwise = if y <= n then 1 + eval (x:xs) ys     (n - y) else 0
