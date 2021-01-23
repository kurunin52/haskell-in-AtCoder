module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do 
    putStrLn "someFunc"
    main

main :: IO ()
main = do
    let d = Node "hoge"
    let d2 = Branch d "hh" (Branch (Node "fuga") "ee" (Node "piyo"))
    -- putStrLn (show d2)
    print d2
    putStrLn "Hello World!"

data Tree a
    = Node a 
    | Branch (Tree a) a (Tree a)
    deriving Show
