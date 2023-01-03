module Day_2022_11 where
import Data.List (partition, groupBy, sortBy, sort, group)
import Data.Function (on)
import Control.Arrow ((>>>), first, second)
import Helpers
import Text.Parsec
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft))
import qualified Text.Parsec.Token as P
import Control.Applicative (liftA2)


type Item   = Int
type Id     = Int
type Monkey = [Item] -> [(Id, Item)]

type Parser = Parsec String ()

monkey :: (Item -> Item) -> (Item -> Bool) -> Id -> Id -> Monkey
monkey operation test whenTrue whenFalse = foldr go []
    where go item =
              let new = operation item `div` 3
              in if test new
                 then ((whenTrue, new) :)
                 else ((whenFalse, new) :)

run :: [(Id, Monkey)] -> [(Id, Item)] -> [([(Id, Item)], [(Id, Item)])]
run monkeys items = iterate
                      (resetCounter . foldr1 (>>>) . map go $ monkeys)
                      ([], items) -- add counter stream
    where resetCounter fn (_, b) = fn ([], b) -- iterate captures per-round, so flush
          go (id', monkey') (countingStream, state) = -- counting and state-passing
              let (these, those) = partition (\(i,_) -> i == id') state
              in  (these ++ countingStream, those ++ monkey' (map snd these))

parser :: Parser ([(Id, Monkey)], [(Id, Item)])
parser = foldr go ([], []) <$> many1 monkeyDef
    where go (id', monkey', items) (ms, is) = ((id', monkey') : ms, map (id',) items ++ is)

monkeyDef :: Parser (Id, Monkey, [Item])
monkeyDef = go <$ symbol "Monkey" <*> positiveNatural <* symbol ":"
               <* symbol "Starting items:"           <*> lexeme (commaSep positiveNatural)
               <* symbol "Operation: new ="          <*> operationP "old"
               <* symbol "Test: divisible by"        <*> lexeme (flip divisible <$> positiveNatural)
               <* symbol "If true: throw to monkey"  <*> lexeme positiveNatural
               <* symbol "If false: throw to monkey" <*> lexeme positiveNatural
    where go me starters operation test true false = (me, monkey operation test true false, starters)

-- parser for simple operations. supports positive and negative numbers,
-- postfix ++, addition, subtraction, multiplication, integer division,
-- (nested) parentheses and a single variable
operationP :: String -> Parser (Item -> Item)
operationP varName = buildExpressionParser table term
    where table = [ [prefix "-" (negate .), prefix "+" (id .)]
                  , [postfix "++" ((+1) .)]
                  , [binary "*" (liftA2 (*)) AssocLeft, binary "/" (liftA2 div) AssocLeft]
                  , [binary "+" (liftA2 (+)) AssocLeft, binary "-" (liftA2 (-)) AssocLeft]
                  ]
          term = P.parens lexer (operationP varName)
             <|> const <$> lexeme positiveNatural
             <|> id    <$  symbol varName

main :: String -> Either ParseError ([(Id, Int)], [[[Item]]], Int)
main = fmap ( uncurry monkeyBusiness
            . first countItemsPerMonkey
            . unzip
            . map (second perRoundPerMonkey)
            . take 21 -- round 0 represents initial state, so we need one more
            . uncurry run
            )
     . parse parser ""
  where countItemsPerMonkey = map (\x -> (head x, length x)) . group . sort . map fst . concat
        perRoundPerMonkey   = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
        monkeyBusiness a b  = (a, b, product . take 2 . sortBy (flip compare) . map snd $ a)