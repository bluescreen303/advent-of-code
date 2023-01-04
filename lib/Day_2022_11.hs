module Day_2022_11 where
import Data.List (partition, groupBy, sortBy)
import Data.Function (on)
import Control.Arrow ((>>>), (***))
import Helpers
import Text.Parsec
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft))
import Control.Applicative (liftA2)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Item   = Integer
type Id     = Int
type Monkey = [Item] -> [(Id, Item)]
type Parser = Parsec String ()

monkey :: (Item -> Item) -> (Item -> Bool) -> Id -> Id -> Monkey
monkey operation test whenTrue whenFalse = foldr go []
    where go item =
              let new = operation item
              in if test new
                 then ((whenTrue, new) :)
                 else ((whenFalse, new) :)

run :: [(Id, Monkey)] -> [(Id, Item)] -> [(IntMap Int, [(Id, Item)])]
run monkeys items = iterate
                      (foldr1 (>>>) . map go $ monkeys)
                      (IntMap.empty, items)    -- add counting
    where go (id', monkey') (!counts, state) = -- counting and state-passing
              let (these, those) = partition (\(i,_) -> i == id') state
              in  (IntMap.insertWith (+) id' (length these) counts, those ++ monkey' (map snd these))

parser :: Parser ([(Id, Monkey)], [(Id, Item)])
parser = foldr go ([], []) <$> many1 monkeyP
    where go (id', monkey', items) (ms, is) = ((id', monkey') : ms, map (id',) items ++ is)

monkeyP :: Parser (Id, Monkey, [Item])
monkeyP = go <$ symbol "Monkey" <*> positiveNatural <* symbol ":"
             <* symbol "Starting items:"           <*> lexeme (commaSep (toInteger <$> positiveNatural))
             <* symbol "Operation: new ="          <*> operationP "old"
             <* symbol "Test: divisible by"        <*> lexeme (flip divisible . toInteger <$> positiveNatural)
             <* symbol "If true: throw to monkey"  <*> lexeme positiveNatural
             <* symbol "If false: throw to monkey" <*> lexeme positiveNatural
    where go !me starters !operation !test !true !false = (me, monkey operation test true false, starters)

-- parser for simple operations. supports positive and negative numbers,
-- postfix ++, addition, subtraction, multiplication, integer division,
-- (nested) parentheses and a single variable
operationP :: String -> Parser (Item -> Item)
operationP varName = go
    where go = buildExpressionParser table term
          table = [ [prefix "-" (negate .), prefix "+" (id .)]
                  , [postfix "++" ((+1) .)]
                  , [binary "*" (liftA2 (*)) AssocLeft, binary "/" (liftA2 div) AssocLeft]
                  , [binary "+" (liftA2 (+)) AssocLeft, binary "-" (liftA2 (-)) AssocLeft]
                  ]
          term = parens go
             <|> const <$> lexeme (toInteger <$> positiveNatural)
             <|> id    <$  symbol varName

main :: String -> Either ParseError ([(Id, Int)], [[[Item]]], Int)
main = fmap ( uncurry monkeyBusiness
            . (countItemsPerMonkey *** map perRoundPerMonkey)
            . unzip
            . take 1001 -- round 0 represents initial state, so we need one more
            . uncurry run
            )
     . parse parser ""
  where countItemsPerMonkey = IntMap.toList . last
        perRoundPerMonkey   = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
        monkeyBusiness a b  = (a, b, product . take 2 . sortBy (flip compare) . map snd $ a)