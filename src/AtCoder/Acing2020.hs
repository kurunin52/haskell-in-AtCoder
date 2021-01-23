module Acing2020 where

-- import System.IO

_a :: IO ()
_a = do
    [l, r, d] <- map (read :: String -> Int) . words <$> getLine
    print $ sum $ [(1 :: Int) | x <- [l..r], x `mod` d == 0]

_b :: IO ()
_b = do
    _ <- readLn :: IO Int
    as <- map (read :: String -> Int) . words <$> getLine
    let as_ = [x | (x, _) <- zip as [(1 :: Int)..], odd x]
    print $ sum $ [(1 :: Int) | x <- as_, odd x]

_c :: IO ()
_c = do
    n <- (readLn :: IO Double)
    let pat = map patterns [1..n]
    mapM_ ((print :: Int -> IO ()) . validate) (zip pat [(1 :: Int)..])
        where
            patterns n = let up = floor $ sqrt n in [(x,y,z) | x <- [1..up], y <- [x..up], z <- [y..up]]
            validate :: (Eq a1, Num a2, Num a1) => ([(a1, a1, a1)], a1) -> a2
            validate (pats, n) = sum $ [ if x == y && y == z then 1 else if x /= y && y /= z then 6 else 3  | (x, y, z) <- filter (validate_ n) pats]
            validate_ n (x, y, z) = ((x + y + z) ^ (2 :: Int)) - x * y - y * z - z * x == n
