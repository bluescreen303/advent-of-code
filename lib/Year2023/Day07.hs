module Year2023.Day07 where

import Data.Bifunctor (first)
import Data.Coerce (coerce, Coercible)
import Data.List (sort, group, sortBy)
import Numeric.Natural (Natural)
import Text.Parsec

import Helpers

main :: Bool -> String -> Either ParseError Natural
main False = fmap (puzzle @Card)    . doParse parser
main True  = fmap (puzzle @AltCard) . doParse parser

puzzle :: ToHand c => [([c], Natural)] -> Natural
puzzle = sum . zipWith (\rank (_, bid) -> rank * bid) [1..] . sort . map (first toHand)

data Card = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | Jack | Queen | King | Ace
     deriving (Show, Enum, Eq, Ord)

newtype AltCard = AltCard Card deriving (Show, Eq)

instance Ord AltCard where
      (AltCard Jack) `compare` (AltCard Jack) = EQ
      (AltCard Jack) `compare` _              = LT
      _              `compare` (AltCard Jack) = GT
      (AltCard a)    `compare` (AltCard b)    = a `compare` b

data Hand c = HighCard [c]
            | OnePair [c]
            | TwoPair [c]
            | ThreeOfAKind [c]
            | FullHouse [c]
            | FourOfAKind [c]
            | FiveOfAKind [c]
            deriving (Show, Eq, Ord)

toHand' :: Ord b => [b] -> [a] -> Hand a
toHand' cards = case sortBy (flip compare) . map length . group . sort $ cards of
                  (5:_)   -> FiveOfAKind
                  (4:_)   -> FourOfAKind
                  (3:2:_) -> FullHouse
                  (3:_)   -> ThreeOfAKind
                  (2:2:_) -> TwoPair
                  (2:_)   -> OnePair
                  _       -> HighCard

class (Coercible c Card, Ord c) => ToHand c where
  toHand :: [c] -> Hand c

instance ToHand Card where
  toHand cards = toHand' cards cards

instance ToHand AltCard where
  toHand cards = maximum . map (`toHand'` cards) . traverse joker $ cards

joker :: AltCard -> [Card]
joker (AltCard Jack) = [(N2)..Ace]
joker (AltCard x)    = [x]

parser :: Coercible Card c => Parser [([c], Natural)]
parser = line `sepEndBy1` endOfLine <* eof
  where line = (,) <$> optSpaces hand <*> optSpaces positiveNatural'
        hand = count 5 (coerce <$> card)
        card = N2 <$ char '2' <|> N3 <$ char '3' <|> N4 <$ char '4' <|> N5 <$ char '5'
           <|> N6 <$ char '6' <|> N7 <$ char '7' <|> N8 <$ char '8' <|> N9 <$ char '9'
           <|> N10 <$ char 'T' <|> Jack <$ char 'J' <|> Queen <$ char 'Q'
           <|> King <$ char 'K' <|> Ace <$ char 'A'
