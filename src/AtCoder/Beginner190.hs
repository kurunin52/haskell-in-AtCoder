module Beginner190 where

-- import Control.Monad (forM_, replicateM)
-- import Control.Monad.ST.Strict ( runST )
-- import Data.STRef (readSTRef)
-- import Data.STRef.Strict ( modifySTRef, newSTRef )
import Control.Monad (replicateM)

_a :: IO ()
_a = do
    [a, b, c] <- map (read :: String -> Int) . words <$> getLine
    putStrLn $
        if c == 0 then
            if a <= b then "Aoki"
            else "Takahashi"
        else 
            if b <= a then "Takahashi"
            else "Aoki"

_b :: IO ()
_b = do
    [n, s, d] <- map (read :: String -> Int) . words <$> getLine
    xs <- map (map (read :: String -> Int) . words) <$> replicateM n getLine
    putStrLn $ if any (check s d) xs then "Yes" else "No"
        where 
            check s d spell = (spell !! 0) < s && (spell !! 1) > d

_c :: IO ()
_c = do
    [n, m] <- map (read :: String -> Int) . words <$> getLine
    conditions <- map (toTaple . map (read :: String -> Int) . words) <$> replicateM m getLine
    k <- (read :: String -> Int) <$> getLine
    cds <- map (toTaple . map (read :: String -> Int) . words) <$> replicateM k getLine
    print $ maximum $ [ length $ filter (check selector) conditions  | selector <- toSelectors cds]
        where 
            toTaple xs = (xs !! 0, xs !! 1)
            toSelectors [x] = [[fst x], [snd x]]
            toSelectors (x:xs) = let next = toSelectors xs in map (fst x :) next ++ map (snd x : ) next
            toSelectors [] = []
            check xs (a, b) = contains xs a && contains xs b 
            contains [] _ = False
            contains (x:xs) y = x == y || contains xs y
