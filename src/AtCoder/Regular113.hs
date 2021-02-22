module Beginner173 where

-- import Data.List
-- import Control.Monad (replicateM)

-- import Control.Monad (forM_, replicateM)
-- import Control.Monad.ST.Strict ( runST )
-- import Data.STRef (readSTRef)
-- import Data.STRef.Strict ( modifySTRef, newSTRef )

_a :: IO ()
_a = do
  k <- (read :: String -> Double) <$> getLine
  -- let list = [(x, y, z) | x <- [1 .. floorDouble (k ** (1 / 3))], y <- [x .. floorDouble $ sqrt k], z <- [y .. floorDouble (k / fromInteger x / fromInteger y)]]
  -- print $ sum [patternize x y z | (x, y, z) <- list]
  let list = [patternize x y z | x <- [1 .. floorDouble (k ** (1 / 3))], y <- [x .. floorDouble $ sqrt k], z <- [y .. floorDouble (k / fromInteger x / fromInteger y)]]
  print $ sum list
  where
    floorDouble :: Double -> Integer
    floorDouble = floor
    patternize :: Integer -> Integer -> Integer -> Int
    patternize x y z
      | x == y =
        if y == z
          then 1
          else 3
      | y == z = 3
      | otherwise = 6
