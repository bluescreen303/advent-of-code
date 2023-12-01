module Year2022.Day06 where

import Data.List (tails, nub)

main :: String -> [(String, Int)]
main = map (\line -> (line, snd $ startPos line))
     . lines

startPos :: String -> (String, Int)
startPos input = (head b, length a + 14)
    where (a,b)   = break uniqs . tails $ input
          uniqs q = length (nub (take 14 q)) == 14
