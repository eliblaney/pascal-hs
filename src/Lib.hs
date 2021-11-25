module Lib
    ( pascal, pascalStr
    ) where

import Data.List

factorial :: Int -> Int
factorial n = product [1..n]

comb :: Int -> Int -> Int
comb n k = (factorial n) `div` ((factorial k) * factorial (n-k))

pascalRow :: Int -> [Int]
pascalRow n = [comb n k | k <- [0..n]]

pascalRowStr :: Int -> String
pascalRowStr n = intercalate " " $ map show $ pascalRow n

pascal :: [[Int]]
pascal = [1] : [pascalRow i | i <- [1..]]

showInts :: [Int] -> String
showInts xs = intercalate " " $ map show xs

pascalStr :: Int -> String
pascalStr n = intercalate "\n" $ map show $ map showInts $ take n pascal
