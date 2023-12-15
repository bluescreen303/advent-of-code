module Year2023.Day15 (main) where

import Text.Parsec hiding (label)
import Helpers

import Data.Char (ord)
import Data.List (foldl')
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as V

main :: Bool -> String -> Either ParseError Int
main secondPuzzle = fmap handler . doParse parser . filter (/= '\n')
  where handler | secondPuzzle = solve
                | otherwise    = sum . map (hash . show)

hash :: String -> Int
hash = foldl' (\n c -> (n + ord c) * 17 `mod` 256) 0

data Instruction = Add    { label :: String, strength :: Int }
                 | Remove { label :: String }

instance Show Instruction where
  show i@(Add _ _)  = label i ++ '=' : show (strength i)
  show i@(Remove _) = label i ++ "-"

parser :: Parser [Instruction]
parser = instruction `sepBy1` char ',' <* eof
  where instruction = flip ($) <$> many1 letter <*> inst
        inst = flip Add    <$ char '=' <*> positiveNatural
                <|> Remove <$ char '-'

newtype HashMap = HashMap { vec :: Vector [(String, Int)] }

action :: HashMap -> Instruction -> HashMap
action (HashMap v) i = let n = hash $ label i in HashMap (v // [(n, modify (v ! n))])
  where modify = case i of
          (Add l s)  -> uncurry ($) . foldr (go (l,s)) (((l,s):), [])
          (Remove l) -> filter ((/= l) . fst)
        go replacement original (fn, rest)
          | fst replacement == fst original = (id, replacement:rest)
          | otherwise                       = (fn,    original:rest)

solve :: [Instruction] -> Int
solve = sum . zipWith go [1..] . V.toList . vec . foldl' action (HashMap $ V.replicate 256 [])
  where go boxNo = sum . zipWith (\slotNo (_, focalLength) -> boxNo * slotNo * focalLength) [1..] . reverse
