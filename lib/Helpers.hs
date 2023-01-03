module Helpers where

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
argOr dataFileName = do
    args <- getArgs
    case args of
        [f] -> return f
        _   -> getDataFileName dataFileName

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

-- parsing utilities

positiveNatural :: Stream s m Char => ParsecT s u m Int
positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser L.emptyDef

lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = P.lexeme lexer

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = P.commaSep lexer

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary  name fun = Infix   (do { P.reservedOp lexer name; return fun })

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix  name fun = Prefix  (do { P.reservedOp lexer name; return fun })

postfix :: String -> (a -> a) -> Operator String u Identity a
postfix name fun = Postfix (do { P.reservedOp lexer name; return fun })
