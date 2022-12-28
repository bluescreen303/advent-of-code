module Day_2022_05 where

import Data.Maybe (catMaybes)
import Data.Char (digitToInt)
import Data.List (foldl', transpose)
import Text.Printf (printf)
import Text.Parsec

newtype Crate = Crate Char deriving Eq

instance Show Crate where
    show (Crate c) = printf "[%c]" c

newtype Stacks = Stacks [[Crate]] deriving Eq

boxed :: Stacks -> [[Maybe Crate]]
boxed (Stacks xs) = reverse
                  . transpose
                  . map (take size . (++ repeat Nothing) . map Just . reverse)
                  $ xs
    where size = maximum . map length $ xs

unboxed :: [[Maybe Crate]] -> Stacks
unboxed = Stacks
        . map (reverse . catMaybes)
        . transpose
        . reverse

top :: Stacks -> String
top (Stacks xs) = map go xs
    where go []            = ' '
          go (Crate x : _) = x

instance Show Stacks where
    show s@(Stacks xs) = unlines
                       . (++ [footer])
                       . map ( unwords
                             . map (maybe "   " show)
                             )
                       . boxed
                       $ s
        where footer :: String
              footer = unwords
                     . map (printf " %d ")
                     $ [1..(length xs)]

grab :: Stacks -> Int -> Maybe (Stacks, Crate)
grab (Stacks xs) n = case splitAt (n - 1) xs of
    (before, (item:rest):after) -> Just (Stacks (before ++ rest : after), item)
    _                           -> Nothing

put :: Stacks -> Int -> Crate -> Maybe Stacks
put (Stacks xs) n item = case splitAt (n - 1) xs of
    (before, stack:after) -> Just (Stacks (before ++ (item : stack) : after))
    _                     -> Nothing

moveOne :: Int -> Int -> Stacks -> Maybe Stacks
moveOne from' to' s = do
    (s', c) <- grab s from'
    put s' to' c

data Move = Move { amount :: Int
                 , from   :: Int
                 , to     :: Int
                 }
                 deriving Eq

instance Show Move where
    show m = printf "move %d from %d to %d" (amount m) (from m) (to m)

apply :: Move -> Stacks -> Maybe Stacks
apply (Move a f t) s = foldl' (>>=) (Just s) (replicate a (moveOne f t))

data Puzzle = Puzzle { stacks :: Stacks
                     , moves  :: [Move]
                     }
                     deriving Eq

instance Show Puzzle where
    show p = show (stacks p) ++ "\n" ++ unlines (map show $ moves p)

step :: Puzzle -> Maybe Puzzle
step (Puzzle _ [])     = Nothing
step (Puzzle s (m:ms)) = flip Puzzle ms <$> apply m s

run :: Puzzle -> Maybe Stacks
run (Puzzle s ms) = foldl' ((. apply) . (>>=)) (Just s) ms

type Parser = Parsec String ()

puzzleParser :: Parser Puzzle
puzzleParser = Puzzle <$> stacksParser <* endOfLine
                      <*> moveParser `sepEndBy` endOfLine

stacksParser :: Parser Stacks
stacksParser = unboxed <$> (try (maybeCrateParser `sepBy1` char ' ') `endBy1` endOfLine)
                       <*  (between (char ' ') (char ' ') digit `sepBy1` char ' ')
                       <*  endOfLine

maybeCrateParser :: Parser (Maybe Crate)
maybeCrateParser = (Just <$> between (char '[') (char ']') (Crate <$> letter))
               <|> (Nothing <$ string "   ")

moveParser :: Parser Move
moveParser = Move <$  string "move "
                  <*> positiveNatural
                  <*  string " from "
                  <*> positiveNatural
                  <*  string " to "
                  <*> positiveNatural

positiveNatural :: Parser Int
positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

doParse :: String -> Either ParseError Puzzle
doParse = parse (puzzleParser <* eof) ""

main :: String -> Either ParseError (Maybe String)
main = fmap (fmap top . run)
     . doParse