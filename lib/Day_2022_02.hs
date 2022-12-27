module Day_2022_02 where

-- Rock Paper Scissors is a game between two players.
-- Each game contains many rounds; in each round,
-- the players each simultaneously choose one of Rock,
-- Paper, or Scissors using a hand shape.
data Shape = Rock
           | Paper
           | Scissors
           deriving Eq

-- Then, a winner for that round is selected:
-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock. 
-- If both players choose the same shape, the round instead ends in a draw.
battle :: Shape -> Shape -> Result
battle h1       h2
    | h1 == h2           = Draw
battle Rock     Scissors = Win
battle Scissors Paper    = Win
battle Paper    Rock     = Win
battle _        _        = Defeat

data Result = Win
            | Draw
            | Defeat

-- Appreciative of your help yesterday, one Elf gives you an encrypted
-- strategy guide (your puzzle input) that they say will be sure to help
-- you win. "The first column is what your opponent is going to play: 
-- A for Rock, B for Paper, and C for Scissors. 
-- The second column, you reason, must be what you should play in response:
--  X for Rock, Y for Paper, and Z for Scissors. 
data OtherSide = A | B | C deriving (Show, Read, Eq)
data OurSide   = X | Y | Z deriving (Show, Read, Eq)

class Decrypt n where
     decrypt :: n -> Shape

instance Decrypt OtherSide where
     decrypt A = Rock
     decrypt B = Paper
     decrypt C = Scissors

instance Decrypt OurSide where
     decrypt X = Rock
     decrypt Y = Paper
     decrypt Z = Scissors

-- The score for a single round is the score for the shape you selected
-- (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the
-- outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
roundScore :: Shape -> Result -> Integer
roundScore s r = shapeScore s + resultScore r

shapeScore :: Shape -> Integer
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

resultScore :: Result -> Integer
resultScore Win    = 6
resultScore Draw   = 3
resultScore Defeat = 0

parseLine :: String -> (OtherSide, OurSide)
parseLine line = case words line of
                    [x, y] -> (read x, read y)
                    e      -> error ("unreadable line" ++ show e)

parseStrategyGuide :: String -> [(OtherSide, OurSide)]
parseStrategyGuide = map parseLine . lines

playGuide :: [(OtherSide, OurSide)] -> Integer
playGuide = sum
          . map (\(them, us) -> roundScore us (battle us them))
          . map (\(them, us) -> (decrypt them, decrypt us))

main :: String -> Integer
main = playGuide . parseStrategyGuide