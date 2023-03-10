module Helpers ( argOr, argOr'
               , binary
               , commaSep
               , divisible
               , grouped
               , merge
               , mergeAll
               , without
               , lexeme
               , parens
               , positiveNatural
               , parseInt
               , postfix
               , prefix
               , splitOn
               , splitPer
               , symbol ) where

import Paths_advent_of_code (getDataFileName)
import System.Environment (getArgs)
import Text.Parsec
import Data.List (foldl')
import Data.Char (digitToInt)

import Data.Functor.Identity (Identity)
import Text.Parsec.Expr (Operator (..), Assoc)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

splitAtEmpty :: [String] -> [[String]]
splitAtEmpty l = case break (== "") l of
                     (items, "":rest) -> items : splitAtEmpty rest
                     (items, _)       -> [items]

grouped :: String -> [(Integer, [String])]
grouped = zip [0..] . splitAtEmpty . lines

argOr :: FilePath -> IO String
argOr = fmap fst . argOr' 0

argOr' :: Int -> FilePath -> IO (String, [String])
argOr' skip dataFileName = do
    (otherArgs, fileArgs) <- splitAt skip <$> getArgs
    case fileArgs of
        [f] -> return (f, otherArgs)
        _   -> (,otherArgs) <$> getDataFileName dataFileName

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn n xs = case break (== n) xs of
                      (items, _:rest) -> items : splitOn n rest
                      (items, _)      -> [items]

splitPer :: Int -> [a] -> [[a]]
splitPer n xs = let (this, that) = splitAt n xs
                in this : case that of
                            [] -> []
                            _  -> splitPer n that

divisible :: Integral a => a -> a -> Bool
divisible x y = x `mod` y == 0

-- set-like behaviour on de-duplicated ordered lists

-- expects de-duplicated, ordered input lists
merge :: Ord a => [a] -> [a] -> [a]
merge []         ys         = ys
merge xs         []         = xs
merge xxs@(x:xs) yys@(y:ys) = case x `compare` y of
    LT -> x : merge xs yys
    EQ -> x : merge xs ys
    GT -> y : merge xxs ys

-- expects de-duplicated, ordered input lists
mergeAll :: Ord a => [[a]] -> [a]
mergeAll = foldr merge []

-- expects de-duplicated, ordered input lists
without :: Ord a => [a] -> [a] -> [a]
without []         _          = []
without xs         []         = xs
without xxs@(x:xs) yys@(y:ys) = case x `compare` y of
    LT -> x : without xs yys
    EQ -> without xs ys
    GT -> without xxs ys

-- parsing utilities

positiveNatural :: Stream s m Char => ParsecT s u m Int
positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

parseInt :: Stream s m Char => ParsecT s u m Int
parseInt = ($) <$> id `option` (negate <$ char '-') <*> positiveNatural

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser L.emptyDef

lexeme :: Parsec String u a -> Parsec String u a
lexeme = P.lexeme lexer

symbol :: String -> Parsec String u String
symbol = P.symbol lexer

commaSep :: Parsec String u a -> Parsec String u [a]
commaSep = P.commaSep lexer

reservedOp :: String -> Parsec String u ()
reservedOp = P.reservedOp lexer

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary  name fun = Infix   (do { reservedOp name; return fun })

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix  name fun = Prefix  (do { reservedOp name; return fun })

postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun = Postfix (do { reservedOp name; return fun })

parens :: Parsec String u a -> Parsec String u a
parens = P.parens lexer
