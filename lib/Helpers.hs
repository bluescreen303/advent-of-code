module Helpers ( Parser
               , argOr, argOr'
               , binary
               , commaSep
               , divisible
               , grouped
               , merge
               , mergeAll
               , without
               , enum
               , withSecondLine
               , mapRSet
               , lowestInRange
               , getDataFileName
               , doParse
               , lexeme
               , parens
               , positiveNatural
               , positiveNatural'
               , parseInt
               , postfix
               , prefix
               , splitOn
               , splitPer
               , takeWithLoopDetection
               , symbol
               , sym
               , optSpaces) where

import qualified Paths_advent_of_code (getDataFileName)
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)
import Data.List (foldl', scanl', find)
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)
import System.FilePath (joinPath)

import Data.Functor.Identity (Identity)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HashMap
import Data.Ranged (RSet (rSetRanges), Range (..), DiscreteOrdered, Boundary (..), makeRangedSet)
import GHC.Natural
import Text.Parsec
import Text.Parsec.Expr (Operator (..), Assoc)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L

grouped :: String -> [(Integer, [String])]
grouped = zip [0..] . splitOn "" . lines

argOr :: Int -> FilePath -> IO String
argOr year = fmap fst . argOr' 0 year

argOr' :: Int -> Int -> FilePath -> IO (String, [String])
argOr' skip year dataFileName = do
    (otherArgs, fileArgs) <- splitAt skip <$> getArgs
    case fileArgs of
        [f] -> return (f, otherArgs)
        _   -> (,otherArgs) <$> getDataFileName year dataFileName

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn n xs = case break (== n) xs of
                      (items, _:rest) -> items : splitOn n rest
                      (items, _)      -> [items]

splitPer :: Int -> [a] -> [[a]]
splitPer n xs = let (this, that) = splitAt n xs
                in this : case that of
                            [] -> []
                            _  -> splitPer n that

takeWithLoopDetection :: forall a. Hashable a => Int -> [a] -> a
takeWithLoopDetection n input = snd . fst $ (previous !! n')
  where numbered :: [(Int, a)]
        numbered = zip [0..] input

        previous :: [((Int, a),Maybe Int)]
        previous = zip numbered
                 . tail
                 . map snd
                 . scanl' (\(m, _) (i, value) ->
                              (HashMap.insert value i m, HashMap.lookup value m))
                          (HashMap.empty, Nothing)
                 $ numbered

        n' = let (end, start) = bimap fst fromJust . fromJust . find (isJust . snd) $ previous
             in ((n - start) `mod` (end - start)) + start

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

enum :: (Bounded a, Enum a) => [a]
enum = enumFromTo minBound maxBound

withSecondLine :: (a -> a) -> [a] -> [a]
withSecondLine fn (first:second:rest) = first : fn second : rest
withSecondLine _  _                   = error "withSecondLine called without second line"

-- Range extensions

-- Ranges and RSets are not Functors and come without fmap equivalent
mapRSet :: (DiscreteOrdered a, DiscreteOrdered b) => (a -> b) -> RSet a -> RSet b
mapRSet op = makeRangedSet . map mapRange . rSetRanges
  where mapRange (Range l u) = Range (mapBoundary l) (mapBoundary u)
        mapBoundary (BoundaryAbove x) = BoundaryAbove (op x)
        mapBoundary (BoundaryBelow x) = BoundaryBelow (op x)
        mapBoundary BoundaryAboveAll  = BoundaryAboveAll
        mapBoundary BoundaryBelowAll  = BoundaryBelowAll

lowestInRange :: Ord v => Range v -> v
lowestInRange x = case rangeLower x of
                    (BoundaryBelow n) -> n
                    _                 -> error "unsupported Range structure"

--

getDataFileName :: Int -> FilePath -> IO FilePath
getDataFileName year x = Paths_advent_of_code.getDataFileName $ joinPath [show year, x]


-- parsing utilities
type Parser = Parsec String ()

doParse :: Parser x -> String -> Either ParseError x
doParse = flip parse ""

positiveNatural :: Stream s m Char => ParsecT s u m Int
positiveNatural = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

positiveNatural' :: Stream s m Char => ParsecT s u m Natural
positiveNatural' = foldl' (\a i -> a * 10 + (fromIntegral . digitToInt $ i)) 0 <$> many1 digit

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

sym :: String -> Parsec String u String
sym = optSpaces . string

optSpaces :: Parsec String u a -> Parsec String u a
optSpaces p = p <* many (char ' ')
