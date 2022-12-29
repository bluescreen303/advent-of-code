module Day_2022_06 where

import Data.List (tails, nub)

main :: String -> [(String, Int)]
main = map (\line -> (line, snd $ startPos line))
     . lines

startPos :: String -> (String, Int)
startPos input = (head b, length a + 4)
    where (a,b)       = break fourUniqs . tails $ input
          fourUniqs q = length (nub (take 4 q)) == 4
