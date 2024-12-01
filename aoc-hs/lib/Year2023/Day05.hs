module Year2023.Day05 (main) where

import Data.Ranged.Ranges
import Text.Parsec
import Helpers

import Data.List (scanl', sortOn)
import Data.Ranged (Boundary(..), makeRangedSet, RSet (rSetRanges), rSetUnion, rSetDifference, rSetIntersection)

main :: Bool -> String -> Either ParseError Int
main sndPuzzle = fmap (uncurry handler) . doParse parser
  where handler mappy = lowestInRange
                      . head
                      . rSetRanges
                      . snd
                      . last
                      . flip journey mappy
                      . makeRangedSet
                      . puzzleSelector
        puzzleSelector
          | sndPuzzle = asRange
          | otherwise = map singletonRange

        asRange []           = []
        asRange (x:len:rest) = Range (BoundaryBelow x) (BoundaryBelow $ x + len) : asRange rest
        asRange _            = error "seeds not given in pairs"


data Mapping = Mapping { range :: Range Int, op :: Int -> Int }
newtype Map = Map [Mapping]

get :: Map -> RSet Int -> RSet Int
get (Map mappings) inputs = nonMappedInputs `rSetUnion` mappedInputs
  where allMappingRanges, nonMappedInputs, mappedInputs :: RSet Int
        allMappingRanges = makeRangedSet $ map range mappings
        nonMappedInputs  = inputs `rSetDifference` allMappingRanges
        mappedInputs     = foldr1 rSetUnion $ map go mappings
        go :: Mapping -> RSet Int
        go m = mapRSet (op m) $ inputs `rSetIntersection` makeRangedSet [range m]

type Journey = [(String, String, Map)]

parser :: Parser (Journey, [Int])
parser = flip (,) <$> section seeds <*> many1 (section mappy) <* eof
  where section p = p <* optional endOfLine
        seeds     = sym "seeds:" *> many1 pos <* endOfLine
        mappy     = (,,) <$> many1 letter <* string "-to-" <*> many1 letter <* sym " map:"
                         <*  endOfLine
                         <*> mapLines
        mapLines  = toMap <$> mapLine `sepEndBy1` endOfLine
        mapLine   = toMapping <$> pos <*> pos <*> pos
        pos       = optSpaces positiveNatural

        toMap                 = Map . sortOn (rangeLower . range)
        toMapping dst src len = Mapping (Range (BoundaryBelow src) (BoundaryBelow $ src + len))
                                        (+(dst-src))

journey :: RSet Int -> Journey -> [(String, RSet Int)]
journey seed = scanl' (\(_, ns) (_, dst, mappy) -> (dst, get mappy ns)) ("seed", seed)
