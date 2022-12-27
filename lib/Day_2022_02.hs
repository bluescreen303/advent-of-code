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
winAgainst :: Shape -> Shape
winAgainst Rock     = Paper
winAgainst Paper    = Scissors
winAgainst Scissors = Rock

loseAgainst :: Shape -> Shape
loseAgainst Rock     = Scissors
loseAgainst Paper    = Rock
loseAgainst Scissors = Paper

against :: Result -> Shape -> Shape
against Draw   = id
against Win    = winAgainst
against Defeat = loseAgainst

battle :: Shape -> Shape -> Result
battle h1       h2
    | h1 == h2            = Draw
    | h1 == winAgainst h2 = Win
battle _        _         = Defeat

data Result = Win
            | Draw
            | Defeat

-- Appreciative of your help yesterday, one Elf gives you an encrypted
-- strategy guide (your puzzle input) that they say will be sure to help
-- you win. "The first column is what your opponent is going to play:
-- A for Rock, B for Paper, and C for Scissors.
-- The Elf finishes helping with the tent and sneaks back over to you.
-- "Anyway, the second column says how the round needs to end:
-- X means you need to lose, Y means you need to end the round in a draw,
-- and Z means you need to win. Good luck!"
data OtherSide = A | B | C deriving (Show, Read, Eq)
data OurSide   = X | Y | Z deriving (Show, Read, Eq)

class Decrypt e d where
     decrypt :: e -> d

instance Decrypt OtherSide Shape where
     decrypt A = Rock
     decrypt B = Paper
     decrypt C = Scissors

instance Decrypt OurSide Result where
     decrypt X = Defeat
     decrypt Y = Draw
     decrypt Z = Win

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
          . map (uncurry roundScore)
          . map (\(theirShape, ourResult) -> (ourResult `against` theirShape, ourResult))
          . map (\(them, us) -> (decrypt them, decrypt us))

main :: String -> Integer
main = playGuide . parseStrategyGuide