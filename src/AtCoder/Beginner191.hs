module Beginner173 where

import Control.Monad (forM_, replicateM)
import Control.Monad.ST.Strict ( runST )
import Data.STRef (readSTRef)
import Data.STRef.Strict ( modifySTRef, newSTRef )

_a :: IO ()
_a = do
    [v, t, s, d] <- map (read :: String -> Int) . words <$> getLine
    putStrLn $ if v * t <= d && d <= v * s then "No" else "Yes"

_b :: IO ()
_b = do
    [_, x] <- words <$> getLine
    as <- filter (/= x) . words <$> getLine
    if null as
        then putStrLn ""
        else do
            putStr $ head as
            forM_ (tail as) $ \a -> do
                putStr " "
                putStr a
            -- forM_ (tail as) (\a -> putStr a)
            -- forM_ (tail as) (\a -> putStr (" " ++ a))
    -- putStrLn $ if null as then "" else foldl1 (\cur b -> cur ++ " " ++ b) as
    -- let as' = [a' | a' <- as, a' /= x] 
    -- putStrLn $ if length as' == 0 then "" else foldl1 (\cur b -> cur ++ " " ++ b) as'

    -- putStrLn $ if (v * t) <= d && d <= (v * s) then "No" else "Yes"


_c :: IO ()
_c = do
    [h, w] <- map (read :: String -> Int) .words <$> getLine
    xs <- replicateM h getLine
    print $ cnt xs h w
        where
            cnt :: [[Char]] -> Int -> Int -> Int
            cnt xs h w = runST $ do
                v <- newSTRef 0
                forM_ [1 .. (h - 2)] $ \h' -> 
                    forM_ [1 .. (w - 2)] $ \w' ->
                        if xs !! h' !! w' == '#' then do
                            let ur = checkUpperRight (xs, h', w')
                            let ul = checkUpperLeft (xs, h', w')
                            let dr = checkDownerRight (xs, h', w')
                            let dl = checkDownerLeft (xs, h', w')
                            if (ur == 1) || (ur == 3) then
                                modifySTRef v id
                                else (if ur == 0 then modifySTRef v (+ 3) else modifySTRef v (+ 1))
                            if (ul == 1) || (ul == 3) then
                                modifySTRef v id
                                else (if ul == 0 then modifySTRef v (+ 3) else modifySTRef v (+ 1))
                            if (dr == 1) || (dr == 3) then
                                modifySTRef v id
                                else (if dr == 0 then modifySTRef v (+ 3) else modifySTRef v (+ 1))
                            if (dl == 1) || (dl == 3) then
                                modifySTRef v id
                                else (if dl == 0 then modifySTRef v (+ 3) else modifySTRef v (+ 1))
                        else modifySTRef v id
                modifySTRef v (`div` 3)
                readSTRef v
            up          (xs, h, w) = xs !! (h - 1)  !! w
            down        (xs, h, w) = xs !! (h + 1)  !! w
            right       (xs, h, w) = xs !! h        !! (w + 1)
            left        (xs, h, w) = xs !! h        !! (w - 1)
            upperRight  (xs, h, w) = xs !! (h - 1)  !! (w + 1)
            upperLeft   (xs, h, w) = xs !! (h - 1)  !! (w - 1)
            downerRight (xs, h, w) = xs !! (h + 1)  !! (w + 1)
            downerLeft  (xs, h, w) = xs !! (h + 1)  !! (w - 1)
            checkUpperRight     t   = (isBlack . up $ t)    + (isBlack . right $ t) + (isBlack . upperRight $ t)
            checkUpperLeft      t   = (isBlack . up $ t)    + (isBlack . left $ t)  + (isBlack . upperLeft $ t)
            checkDownerRight    t   = (isBlack . down $ t)  + (isBlack . right $ t) + (isBlack . downerRight $ t)
            checkDownerLeft     t   = (isBlack . down $ t)  + (isBlack . left $ t)  + (isBlack . downerLeft $ t)
            isBlack x = if x == '#' then 1 else 0

_d :: IO ()
_d = do
    [x, y, r] <- map (read :: String -> Double) . words <$> getLine
    print $ sum [1 |
        x' <- [fromIntegral (round (x - r)) - 1 .. fromIntegral (round (x + r) + 1)],
        y' <- [fromIntegral (round (y - r)) - 1 .. fromIntegral (round (y + r) + 1)],
        sqrt ((abs (x' - x) - abs (y' - y)) ^ 2) <= r]
