module Year2023.Day09 (main) where

import Text.Parsec
import Helpers

main :: Bool -> String -> Either ParseError Int
main secondPuzzle = fmap (sum . map (head . oneResult)) . doParse parser
  where oneResult = foldr1 applyDiff
                  . reverse                      -- process top-to-bottom (starting 0 0 0 0)
                  . takeWhile (not . all (==0))
                  . iterate diffs                -- build stack of diff-lists
                  . if secondPuzzle then id else reverse

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) xs (tail xs)

applyDiff :: [Int] -> [Int] -> [Int]
applyDiff this next = head this + head next : next

parser :: Parser [[Int]]
parser = many1 (optSpaces parseInt) `sepEndBy1` endOfLine <* eof
