module Year2022.Day07 where

import Control.Monad.State hiding (get)
import Data.List (minimumBy)
import Data.Function (on)
import Text.Printf (printf)
import Helpers (splitOn, positiveNatural)

import Text.Parsec hiding (State)

data FileSystemNode = File String Int
                    | Dir String [FileSystemNode]
                    deriving (Show, Eq)

isFile, isDir :: FileSystemNode -> Bool
isFile (File _ _) = True
isFile _          = False
isDir (Dir _ _)   = True
isDir _           = False

hasName :: String -> FileSystemNode -> Bool
hasName x (File n _) = n == x
hasName x (Dir n _)  = n == x

totalSize :: FileSystemNode -> Int
totalSize (File _ s) = s
totalSize (Dir _ cs) = sum $ map totalSize cs

ls' :: FileSystemNode -> String
ls' (Dir _ cs) = unlines . map go $ cs
    where go (File name size) = printf "%d %s" size name
          go (Dir name _)     = printf "dir %s" name
ls' _          = error "cannot ls 'on' a file"

lsTree :: FileSystemNode -> String
lsTree = unlines . go
    where go (File name size)    = [printf "- %s (file, size=%d)" name size]
          go (Dir name contents) = printf "- %s (dir)" name
                                 : map ("  " ++) (concatMap go contents)

data Cxt = Top
         | InDir Cxt String [FileSystemNode] [FileSystemNode]

data WorkDir = WorkDir FileSystemNode Cxt

cdUp :: WorkDir -> WorkDir
cdUp w@(WorkDir _ Top)              = w
cdUp (WorkDir fs (InDir p n bs as)) = WorkDir (Dir n (bs ++ fs : as)) p

cdTop :: WorkDir -> WorkDir
cdTop w@(WorkDir _ Top) = w
cdTop w                 = cdTop (cdUp w)

-- silent/no error handling
changeDir :: String -> WorkDir -> WorkDir
changeDir name w@(WorkDir (Dir n cs) cxt) =
    case break (\x -> isDir x && hasName name x) cs of
        (bs, this:as) -> WorkDir this (InDir cxt n bs as)
        _             -> w
changeDir _ w = w

updateDir :: [FileSystemNode] -> WorkDir -> WorkDir
updateDir cs (WorkDir (Dir n _) cxt) = WorkDir (Dir n cs) cxt
updateDir _  w                       = w

-- stateful walking

newtype FSWalker a = FSWalker (State WorkDir a) deriving (Functor, Applicative, Monad)

cd :: String -> FSWalker ()
cd "/" = FSWalker . modify $ cdTop
cd n   = FSWalker . modify . foldr1 (flip (.)) . map go . splitOn '/' $ n
    where go ".." = cdUp
          go x    = changeDir x

get :: FSWalker FileSystemNode
get = FSWalker . gets $ \(WorkDir d _) -> d

ls :: FSWalker String
ls = ls' <$> get

update :: [FileSystemNode] -> FSWalker ()
update = FSWalker . modify . updateDir

walk :: FileSystemNode -> FSWalker a -> (a, FileSystemNode)
walk n (FSWalker s) = let (result, wd)  = runState s (WorkDir n Top)
                          (WorkDir d _) = cdTop wd
                      in (result, d)

start :: FSWalker a -> (a, FileSystemNode)
start = walk (Dir "/" [])

-- parsing the input

type Parser = Parsec String ()

parser :: Parser (FSWalker ())
parser = foldr1 (>>) <$> many1 (string "$ " *> (cdParser <|> lsParser))
    where cdParser = cd <$ string "cd " <*> manyTill anyChar endOfLine
          lsParser = update <$ string "ls" <* endOfLine <*> many lsLineParser
          lsLineParser = (flip File <$> positiveNatural <* space <*> manyTill anyChar endOfLine)
                        <|>
                         (flip Dir [] <$ string "dir " <*> manyTill anyChar endOfLine)

doParse :: String -> Either ParseError (FSWalker ())
doParse = parse (parser <* eof) ""

---

traverseFS :: FileSystemNode -> [FileSystemNode]
traverseFS f@(File _ _) = [f]
traverseFS d@(Dir _ cs) = d : concatMap traverseFS cs

best :: FileSystemNode -> Int
best fs = totalSize smallest
    where current  = totalSize fs
          needed   = max 0 $ 30000000 - (70000000 - current)
          options  = filter (\x -> isDir x && totalSize x >= needed) . traverseFS $ fs
          smallest = minimumBy (compare `on` totalSize) options

main :: String -> Either ParseError Int
main x = best . snd . start <$> doParse x
