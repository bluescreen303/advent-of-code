module Year2022.Day10 where

import Helpers

data Instruction = Noop
                 | AddX Int

numCycles :: Instruction -> Int
numCycles Noop     = 1
numCycles (AddX _) = 2

data CPU = CPU { instructions :: [Instruction]
               , instCycles   :: Int
               , registerX    :: Int
               }

execute :: Instruction -> CPU -> CPU
execute Noop c     = c
execute (AddX y) c = c { registerX = registerX c + y }

newCPU :: [Instruction] -> CPU
newCPU is = CPU is 0 1

tick :: CPU -> Maybe CPU
tick      (CPU []     _ _) = Nothing
tick self@(CPU (i:is) n _)
    | n == numCycles i -1  = Just (execute i self) { instructions = is, instCycles = 0 }
    | otherwise            = Just self { instCycles = instCycles self + 1 }

iter :: (a -> Maybe a) -> a -> [a]
iter f x = x : maybe [] (iter f) (f x)

sample :: Int -> [a] -> [a]
sample _ []     = []
sample n (x:xs) = x : sample n (drop (n - 1) xs)

xStream :: CPU -> [Int]
xStream = map registerX
        . iter tick

parse :: String -> [Instruction]
parse = map (parseInstr . words) . lines
    where parseInstr ["addx", n] = AddX (read n)
          parseInstr ["noop"]    = Noop
          parseInstr e           = error $ "could not parse instruction " ++ show e

main :: String -> Int
main = sum . map (uncurry (*))
     . sample 40
     . drop 19
     . zip [1..]
     . xStream . newCPU . parse

main2 :: String -> String
main2 = unlines
      . splitPer 40
      . map (\q -> if q then '#' else '.')
      . zipWith dotPixel (concat . repeat $ [0..39])
      . xStream . newCPU . parse

dotPixel :: Int -> Int -> Bool
dotPixel sprite pix = abs (sprite - pix) <= 1
