module Day_2022_10 where

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

signal :: CPU -> [(Int, Int)]
signal = sample 40
       . drop 19
       . zip [1..]
       . map registerX
       . iter tick

parse :: String -> [Instruction]
parse = map (parseInstr . words) . lines
    where parseInstr ["addx", n] = AddX (read n)
          parseInstr ["noop"]    = Noop
          parseInstr e           = error $ "could not parse instruction " ++ show e

main :: String -> Int
main = sum . map (uncurry (*))
     . signal . newCPU . parse
