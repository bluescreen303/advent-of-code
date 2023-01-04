{-# LANGUAGE ViewPatterns #-}
module Day_2022_11 where
import Data.List (partition, sortBy, transpose)
import Data.Function (fix)
import Control.Arrow ((***))
import Helpers
import Text.Parsec
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft))
import Control.Applicative (liftA2)

type Parser = Parsec String ()

type Item  = Integer
type Id    = Int
type Route = (Id, Bool, Id)

-- posessions per round per monkey
type Posessions = [[[Item]]]
-- true and false outputs per round per monkey
type Outputs    = [[([Item], [Item])]]

routesFor :: [Route] -> Int -> [[([a], [a])] -> [a]]
routesFor routes me = map toRoute . filter (\(_, _, to) -> to == me) $ routes
  where toRoute (from, when, _) = roundMod . (if when then fst else snd) . (!! from)
            where roundMod | me >= from = tail
                           | otherwise = id

posessionsFor :: [Route] -> Outputs -> Posessions
posessionsFor routes perMonkey = map itemsForMonkey [0..length perMonkey - 1]
  where itemsForMonkey n = foldr (zipWith (++) . getStream) (repeat [])
                         . filter (\(from, _, to) -> from >= n && to == n) $ routes
        getStream (from, when, _) = (if when then fst else snd)
                                  . unzip . tail $ perMonkey !! from

-- monkey that still needs to learn which monkeys will throw items to it
type DisconnectedMonkey = [[[Item]]] -> ([[Item]], [[Item]])

monkey :: (Item -> Item) -> (Item -> Bool) -> [Item] -> DisconnectedMonkey
monkey oper tst starters inputs = ([] : ts, [] : fs)
    where (ts, fs) = unzip
                   . map ( partition tst
                   . map ((`div` 3) . oper))
                   $ foldr1 (zipWith (++)) ((starters : repeat []) : inputs)

connectMonkeys :: [(Id, DisconnectedMonkey, [Route])] -> (Outputs, Posessions)
connectMonkeys parsed = (map tail result, posessionsFor routes result)
    where (ids, monkees, concat -> routes) = unzip3 parsed
          routing      = routesFor routes
          idMonkees    = zip ids monkees
          network self = map (\(i, m) -> m $ map ($ self) (routing i)) idMonkees
          result       = map (uncurry zip) . fix $ network

main :: Int -> String -> Either ParseError Int
main n = fmap ( fst
              . ( monkeyBusiness . map countActions *** (take n . transpose))
              . connectMonkeys )
         . parse (many1 monkeyP) ""
    where countActions   = sum . take n . map (uncurry (+) . (length *** length))
          monkeyBusiness = product . take 2 . sortBy (flip compare)

monkeyP :: Parser (Id, DisconnectedMonkey, [Route])
monkeyP = go <$ symbol "Monkey" <*> positiveNatural <* symbol ":"
             <* symbol "Starting items:"           <*> lexeme (commaSep (toInteger <$> positiveNatural))
             <* symbol "Operation: new ="          <*> operationP "old"
             <* symbol "Test: divisible by"        <*> lexeme (flip divisible . toInteger <$> positiveNatural)
             <* symbol "If true: throw to monkey"  <*> lexeme positiveNatural
             <* symbol "If false: throw to monkey" <*> lexeme positiveNatural
    where go me starters operation test true false = ( me
                                                     , monkey operation test starters
                                                     , [(me, True, true), (me, False, false)]
                                                     )

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
