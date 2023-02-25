{-# LANGUAGE DataKinds #-}
module Main (main) where

import Test.Hspec
import Data.Either (isRight, fromRight)
import Data.Maybe (fromJust)
import Control.Arrow (second)
import Control.Monad ((>=>), (<=<))
import qualified Data.IntMap as IntMap
import Paths_advent_of_code (getDataFileName)
import qualified Day_2022_01
import qualified Day_2022_02
import Day_2022_02 (OtherSide(..), OurSide(..))
import qualified Day_2022_03
import qualified Day_2022_04
import qualified Day_2022_05
import Day_2022_05 (Crate(..), Stacks(..), Move(..), Puzzle(..))
import qualified Day_2022_06
import qualified Day_2022_07
import Day_2022_07 (FileSystemNode(..), cd, update)
import qualified Day_2022_08
import Grid (Grid, mkGrid, east, west, north, south, mkFocus, value, move, look, topLayer, world)
import qualified Day_2022_09
import qualified Day_2022_10
import qualified Day_2022_11
import qualified Day_2022_12
import Day_2022_12 (Tracking(..))
import qualified Day_2022_13
import Day_2022_13 (Tree(..))
import qualified Day_2022_14
import Day_2022_14 (Vertex (..), Path (..), pathSegments, VertBlock(..), Column(..))
import qualified Day_2022_15
import qualified Day_2022_16

main :: IO ()
main = hspec . parallel $ do

    describe "2022-01" . before (getDataFileName "2022-01-example.txt" >>= readFile) $ do
        describe "the main example" . mapSubject Day_2022_01.main $ do
            it "elf should be 4" $ \(elf, _) ->
                elf + 1 `shouldBe` 4

            it "calories should be 24000" $ \(_, calories) ->
                calories `shouldBe` 24000

        describe "the secondary puzzle" . mapSubject Day_2022_01.main2 $ do
            it "calories should be 24000" $ \calories ->
                calories `shouldBe` 45000

    describe "2022-02" . before (getDataFileName "2022-02-example.txt" >>= readFile) $ do
        describe "the parsed guide" . mapSubject Day_2022_02.parseStrategyGuide $ do
            it "should be as expected" $ \guide ->
                guide `shouldBe` [(A, Y), (B, X), (C, Z)]

        describe "playing the parsed guide" . mapSubject (Day_2022_02.playGuide . Day_2022_02.parseStrategyGuide) $ do
            it "should give a score of 12" $ \score ->
                score `shouldBe` 12

    describe "2022-03" . before (getDataFileName "2022-03-example.txt" >>= readFile) $ do
        describe "priority" $ do
            it "should calculate the right priorities" $ \_ ->
                map Day_2022_03.priority "pLPvts" `shouldBe` [16, 38, 42, 22, 20, 19]
        describe "splitList" $ do
            it "should split a list in halves" $ \_ ->
                Day_2022_03.splitList @Int [1,2,3,4,5,6] `shouldBe` ([1,2,3], [4,5,6])
        describe "the main example" . mapSubject Day_2022_03.main $ do
            it "should calculate a sum of 157" $ \result ->
                result `shouldBe` 157
        describe "the second puzzle" . mapSubject Day_2022_03.main2 $ do
            it "should calculate a sum of 70" $ \result ->
                result `shouldBe` 70

    describe "2022-04" . before (getDataFileName "2022-04-example.txt" >>= readFile) $ do
        describe "the parser" . mapSubject Day_2022_04.doParse $ do
            it "should parse ok" $ \result ->
                result `shouldBe` Right [ ([2,3,4],[6,7,8])
                                        , ([2,3],[4,5])
                                        , ([5,6,7],[7,8,9])
                                        , ([2,3,4,5,6,7,8],[3,4,5,6,7])
                                        , ([6],[4,5,6])
                                        , ([2,3,4,5,6],[4,5,6,7,8])
                                        ]
        describe "the main example" . mapSubject Day_2022_04.main $ do
            it "should find 2 fully contained pairs" $ \result ->
                result `shouldBe` Right 2
        describe "the second puzzle" . mapSubject Day_2022_04.main2 $ do
            it "should find 4 pairs with overlap" $ \result ->
                result `shouldBe` Right 4

    describe "2022-05" $ do
        let exampleStacks = Stacks $ map (map Crate) ["NZ", "DCM", "P"]
        let moves = [ Move 1 2 1
                    , Move 3 1 3
                    , Move 2 2 1
                    , Move 1 1 2
                    ]
        let puzzle = Puzzle exampleStacks moves
        describe "with manual puzzle input" . before (return puzzle) $ do
            describe "boxed" . mapSubject (Day_2022_05.boxed . Day_2022_05.stacks) $ do
                it "should generate bottom-up stacks of same height" $ \xxs ->
                    xxs `shouldBe` [ [Nothing         ,Just (Crate 'D'),Nothing]
                                   , [Just (Crate 'N'),Just (Crate 'C'),Nothing]
                                   , [Just (Crate 'Z'),Just (Crate 'M'),Just (Crate 'P')]]
            describe "run" . mapSubject Day_2022_05.run $ do
                it "should return the correct final state" $ \s ->
                    s `shouldBe` Just (Stacks [ [Crate 'M']
                                              , [Crate 'C']
                                              , [Crate 'D', Crate 'N', Crate 'Z', Crate 'P']])
                describe "top" . mapSubject (fmap Day_2022_05.top) $ do
                    it "should return the correct final state" $ \s ->
                        s `shouldBe` Just "MCD"
        describe "with example input file" . before (getDataFileName "2022-05-example.txt" >>= readFile) $ do
            describe "show" . mapSubject (, show puzzle) $ do
                it "should display the puzzle as in the example" $ \(contents, result) ->
                    result `shouldBe` contents
            describe "parse" . mapSubject Day_2022_05.doParse $ do
                it "should equal the manual input" $ \result ->
                    result `shouldBe` Right puzzle
            describe "main" . mapSubject Day_2022_05.main $ do
                it "should produce the right result" $ \result ->
                    result `shouldBe` Right (Just "MCD")

    describe "2022-06" . before (getDataFileName "2022-06-example.txt" >>= readFile) $ do
        describe "main" . mapSubject Day_2022_06.main $ do
            it "should produce the right result" $ \result ->
                map snd result `shouldBe` [19,23,23,29,26]

    describe "2022-07" $ do
        let exampleFS = Dir "/"
                          [ Dir "a"
                              [ Dir "e"
                                  [ File "i" 584 ]
                              , File "f" 29116
                              , File "g" 2557
                              , File "h.lst" 62596
                              ]
                          , File "b.txt" 14848514
                          , File "c.dat" 8504156
                          , Dir "d"
                              [ File "j" 4060174
                              , File "d.log" 8033020
                              , File "d.ext" 5626152
                              , File "k" 7214296
                              ]
                          ]
        describe "with manual filesystem input" . before (return exampleFS) $ do
            describe "lsTree" . mapSubject Day_2022_07.lsTree $ do
                it "should produce the correct rendering" $ \rendering ->
                    rendering `shouldBe` unlines
                      [ "- / (dir)"
                      , "  - a (dir)"
                      , "    - e (dir)"
                      , "      - i (file, size=584)"
                      , "    - f (file, size=29116)"
                      , "    - g (file, size=2557)"
                      , "    - h.lst (file, size=62596)"
                      , "  - b.txt (file, size=14848514)"
                      , "  - c.dat (file, size=8504156)"
                      , "  - d (dir)"
                      , "    - j (file, size=4060174)"
                      , "    - d.log (file, size=8033020)"
                      , "    - d.ext (file, size=5626152)"
                      , "    - k (file, size=7214296)"
                      ]
            describe "totalSize" . mapSubject Day_2022_07.totalSize $ do
                it "should produce the right total size" $ \result ->
                    result `shouldBe` 48381165
            describe "best" . mapSubject Day_2022_07.best $ do
                it "should return the right number" $ \result ->
                    result `shouldBe` 24933642
            -- describe "walkFS a/e/i" . mapSubject (Day_2022_07.walkFS "a/e/i") $ do
            --     it "should return the right node" $ \result ->
            --         result `shouldBe` Right (File "i" 584)
        let exampleWalker = do
                cd "/"
                update [ Dir "a" []
                       , File "b.txt" 14848514
                       , File "c.dat" 8504156
                       , Dir "d" []
                       ]
                cd "a"
                update [ Dir "e" []
                       , File "f" 29116
                       , File "g" 2557
                       , File "h.lst" 62596
                       ]
                cd "e"
                update [ File "i" 584 ]
                cd "../../a/../d"
                update [ File "j" 4060174
                       , File "d.log" 8033020
                       , File "d.ext" 5626152
                       , File "k" 7214296
                       ]
        describe "with manual filesystem walker input" . before (return exampleWalker) $ do
            describe "start" . mapSubject Day_2022_07.start $ do
                it "should generate the exact same tree as our manual input" $ \(_, result) ->
                    result `shouldBe` exampleFS
        describe "with example input file" . before (getDataFileName "2022-07-example.txt" >>= readFile) $ do
            describe "doParse" . mapSubject Day_2022_07.doParse $ do
                it "should generate the exact same tree as our manual input" $ \result -> do
                    isRight result `shouldBe` True
                    let q = fromRight undefined result
                    snd (Day_2022_07.start q) `shouldBe` exampleFS
            describe "main" . mapSubject Day_2022_07.main $ do
                it "should produce the right result" $ \result -> do
                    isRight result `shouldBe` True
                    let q = fromRight undefined result
                    q `shouldBe` 24933642

    describe "2022-08" $ do
        let exampleGrid :: Grid [3,3] Int = fromJust $ mkGrid [[1,2,3], [4,5,6], [7,8,9]]
        describe "with manual grid input" . before (return . mkFocus $ exampleGrid) $ do
            it "should allow me to traverse the grid in all directions" $ \g ->
                (fmap value . move (east >=> south >=> south >=> west >=> north) $ g)
                `shouldBe`
                Just 4
            it "should allow me to look in all directions" $ \g ->
                let loc = fromJust . move (east >=> south >=> south >=> west >=> north) $ g
                in look east loc `shouldBe` [5,6]
        describe "with example input file" . before (getDataFileName "2022-08-example.txt" >>= readFile) $ do
            describe "main" . mapSubject Day_2022_08.main $ do
                it "should produce the right result" $ \result ->
                    result `shouldBe` Just (21, 8)

    describe "2022-09" $ do
        describe "with example input file" . before (getDataFileName "2022-09-example.txt" >>= readFile) $ do
            describe "main" . mapSubject Day_2022_09.main $ do
                it "should produce the right result" $ \result ->
                    result `shouldBe` 1
        describe "with larger example input file" . before (getDataFileName "2022-09-larger-example.txt" >>= readFile) $ do
            describe "main" . mapSubject Day_2022_09.main $ do
                it "should produce the right result" $ \result ->
                    result `shouldBe` 36

    describe "2022-10" $ do
        describe "with example input file" . before (getDataFileName "2022-10-example.txt" >>= readFile) $ do
            describe "main" . mapSubject Day_2022_10.main $ do
                it "should produce the right result" $ \result ->
                    result `shouldBe` 13140

    describe "2022-11" $ do
        describe "with example input file" . before (getDataFileName "2022-11-example.txt" >>= readFile) $ do
            describe "main" . mapSubject (Day_2022_11.main 20) $ do
                it "should produce the right result" $ \result ->
                    result `shouldBe` Right 10197

    describe "2022-12" $ do
        describe "with example input file" . before (getDataFileName "2022-12-example.txt" >>= readFile) $ do
            describe "with parsed version" . mapSubject (\x -> (x, mkGrid @[5, 8] . Day_2022_12.parse $ x)) $ do
                it "show should recover the input file" $ \(str, parsed) ->
                    fmap show parsed `shouldBe` Just str
                describe "tracked" . mapSubject (Day_2022_12.track <=< snd) $ do
                    let gwOk = Day_2022_12.stepE >=> Day_2022_12.stepE
                    describe "when track is not too steep" . mapSubject (>>= Day_2022_12.liftVisited gwOk) $ do
                        it "should walk ok" $ \result ->
                            fmap (show . topLayer . world) result
                            `shouldBe`
                            Just (unlines [ ">>#....."
                                          , "........"
                                          , "........"
                                          , "........"
                                          , "........" ])
                    let gwBad = gwOk >=> Day_2022_12.stepE
                    describe "when track is too steep" . mapSubject (>>= Day_2022_12.liftVisited gwBad) $ do
                        it "should not step" $ \result -> fmap (show . topLayer . world) result `shouldBe` Nothing
                    describe "solve" . mapSubject (>>= Day_2022_12.solve) $ do
                        it "should solve the correct path" $ \result ->
                            fmap (second show) result `shouldBe`
                                Just (31, unlines [ ">>vv<<<<"
                                                  , "..vvv<<^"
                                                  , "..vv>#^^"
                                                  , "..v>>>^^"
                                                  , "..>>>>>^" ])
            describe "main" . mapSubject Day_2022_12.main $ do
                it "should produce the expected result" $ \result ->
                    result `shouldBe` Just 31
            describe "mainAll" . mapSubject Day_2022_12.mainAll $ do
                it "should produce the expected result" $ \result ->
                    result `shouldBe` Just 29
        describe "with my input" . before (getDataFileName "2022-12-other.txt" >>= readFile) $ do
            describe "parsed and tracked" . mapSubject ((mkGrid @[5, 8] . Day_2022_12.parse) >=> Day_2022_12.track) $ do
                it "should produce the expected tracking map" $ \tracked ->
                    fmap (show . topLayer . world) tracked
                    `shouldBe`
                    Just ( unlines [ "........"
                                   , "..#....."
                                   , "........"
                                   , "........"
                                   , "........"
                                   ])
                it "should put me at the current/start position" $ \tracked ->
                    fmap value tracked `shouldBe` Just Here

    describe "2022-13" $ do
        describe "with example input file" . before (getDataFileName "2022-13-example.txt" >>= readFile) $ do
            let exampleInput = [ ( Node [Leaf 1,Leaf 1,Leaf 3,Leaf 1,Leaf 1]
                                 , Node [Leaf 1,Leaf 1,Leaf 5,Leaf 1,Leaf 1] )
                               , ( Node [Node [Leaf 1],Node [Leaf 2,Leaf 3,Leaf 4]]
                                 , Node [Node [Leaf 1],Leaf 4] )
                               , ( Node [Leaf 9]
                                 , Node [Node [Leaf 8,Leaf 7,Leaf 6]] )
                               , ( Node [Node [Leaf 4,Leaf 4],Leaf 4,Leaf 4]
                                 , Node [Node [Leaf 4,Leaf 4],Leaf 4,Leaf 4,Leaf 4] )
                               , ( Node [Leaf 7,Leaf 7,Leaf 7,Leaf 7]
                                 , Node [Leaf 7,Leaf 7,Leaf 7] )
                               , ( Node []
                                 , Node [Leaf 3] )
                               , ( Node [Node [Node []]]
                                 , Node [Node []] )
                               , ( Node [Leaf 1,Node [Leaf 2,Node [Leaf 3,Node [Leaf 4,Node [Leaf 5,Leaf 6,Leaf 7]]]],Leaf 8,Leaf 9]
                                 , Node [Leaf 1,Node [Leaf 2,Node [Leaf 3,Node [Leaf 4,Node [Leaf 5,Leaf 6,Leaf 0]]]],Leaf 8,Leaf 9] )
                               ]
            describe "parse" . mapSubject Day_2022_13.parse $ do
                it "should produce the example input" $ \result ->
                    result `shouldBe` Right exampleInput
            describe "main" . mapSubject Day_2022_13.main $ do
                it "should produce the answer" $ \result ->
                    result `shouldBe` Right 13
            describe "main2" . mapSubject Day_2022_13.main2 $ do
                it "should produce the answer" $ \result ->
                    result `shouldBe` Right 140

    describe "2022-14" $ do
        describe "pathSegments" $ do
            it "should split a path into separate segments" $ do
                Day_2022_14.pathSegments (Path (Vertex (503,4)) (Vertex (502,4)) [Vertex (502,9),Vertex (494,9)])
                `shouldBe`
                [ (Vertex (503,4),Vertex (502,4))
                , (Vertex (502,4),Vertex (502,9))
                , (Vertex (502,9),Vertex (494,9)) ]
        describe "with example input file" . before (getDataFileName "2022-14-example.txt" >>= readFile) $ do
            let exampleInput = [ Path (Vertex (498,4)) (Vertex (498,6)) [Vertex (496,6)]
                               , Path (Vertex (503,4)) (Vertex (502,4)) [Vertex (502,9),Vertex (494,9)] ]
                jv = Just . Vertex
            describe "parse" . mapSubject Day_2022_14.parse $ do
                it "should produce the example input" $ \result ->
                    result `shouldBe` Right exampleInput
            describe "fromPaths" . mapSubject (const $ Day_2022_14.fromPaths exampleInput) $ do
                it "should produce the expected world" $ \result ->
                    result `shouldBe` Day_2022_14.World 11 (IntMap.fromList [ (494, Column [VertBlock 9 0])
                                                                            , (495, Column [VertBlock 9 0])
                                                                            , (496, Column [VertBlock 6 0, VertBlock 9 0])
                                                                            , (497, Column [VertBlock 6 0, VertBlock 9 0])
                                                                            , (498, Column [VertBlock 4 2, VertBlock 9 0])
                                                                            , (499, Column [VertBlock 9 0])
                                                                            , (500, Column [VertBlock 9 0])
                                                                            , (501, Column [VertBlock 9 0])
                                                                            , (502, Column [VertBlock 4 5])
                                                                            , (503, Column [VertBlock 4 0])
                                                                            ])

                describe "fallOneColumn" $ do
                    describe "the first unit of sand" $ do
                        it "should land at the correct position" $ \world ->
                            Day_2022_14.landed (Day_2022_14.fallOneColumn world (Vertex (500, 0))) `shouldBe` jv (500, 8)
                    describe "a unit of sand too far to the left" $ do
                        it "should land on the infinite bottom" $ \world ->
                            Day_2022_14.landed (Day_2022_14.fallOneColumn world (Vertex (400, 0))) `shouldBe` jv (400, 10)
                    describe "a unit of sand born in a wall" $ do
                        it "should get stuck" $ \world ->
                            Day_2022_14.fallOneColumn world (Vertex (498, 6)) `shouldBe` Day_2022_14.Stuck
                    describe "a unit of sand born just below a wall" $ do
                        it "should land at the correct position" $ \world ->
                            Day_2022_14.landed (Day_2022_14.fallOneColumn world (Vertex (498, 7))) `shouldBe` jv (498, 8)
                describe "fall" $ do
                    describe "the first unit of sand" $ do
                        it "should land at the correct position" $ \world ->
                            Day_2022_14.landed (Day_2022_14.fall world (Vertex (500, 0))) `shouldBe` jv (500, 8)
                    describe "a unit above a wall" $ do
                        it "should land at the correct position" $ \world ->
                            let newWorld = Day_2022_14.World 11 (IntMap.fromList [ (494, Column [VertBlock 9 0])
                                                                                 , (495, Column [VertBlock 9 0])
                                                                                 , (496, Column [VertBlock 6 0, VertBlock 9 0])
                                                                                 , (497, Column [VertBlock 5 1, VertBlock 9 0])
                                                                                 , (498, Column [VertBlock 4 2, VertBlock 9 0])
                                                                                 , (499, Column [VertBlock 9 0])
                                                                                 , (500, Column [VertBlock 9 0])
                                                                                 , (501, Column [VertBlock 9 0])
                                                                                 , (502, Column [VertBlock 4 5])
                                                                                 , (503, Column [VertBlock 4 0])
                                                                                 ])
                            in Day_2022_14.fall world (Vertex (498, 0)) `shouldBe` Day_2022_14.Landed (Vertex (497, 5)) newWorld
                describe "simulation" . mapSubject Day_2022_14.simulation $ do
                    it "should have 94 results" $ \sim ->
                        length sim `shouldBe` 94
                    it "should fill up the correct landing positions" $ \sim ->
                        map Day_2022_14.landed (take 24 sim) `shouldBe` [jv (500,8),jv (499,8),jv (501,8),jv (500,7)
                                                                        ,jv (498,8),jv (499,7),jv (501,7),jv (500,6)
                                                                        ,jv (497,8),jv (498,7),jv (499,6),jv (501,6)
                                                                        ,jv (500,5),jv (499,5),jv (501,5),jv (500,4)
                                                                        ,jv (499,4),jv (501,4),jv (500,3),jv (499,3)
                                                                        ,jv (501,3),jv (500,2),jv (497,5),jv (495,8)]

            describe "main" . mapSubject Day_2022_14.main $ do
                it "should produce the expected result" $ \result ->
                    result `shouldBe` Right 93

    describe "2022-15" $ do
        describe "with example input file" . before (getDataFileName "2022-15-example.txt" >>= readFile) $ do
            let exampleInput = [ Day_2022_15.Sensor (Day_2022_15.Vertex (2,18))  (Day_2022_15.Vertex (-2,15))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (9,16))  (Day_2022_15.Vertex (10,16))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (13,2))  (Day_2022_15.Vertex (15,3))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (12,14)) (Day_2022_15.Vertex (10,16))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (10,20)) (Day_2022_15.Vertex (10,16))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (14,17)) (Day_2022_15.Vertex (10,16))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (8,7))   (Day_2022_15.Vertex (2,10))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (2,0))   (Day_2022_15.Vertex (2,10))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (0,11))  (Day_2022_15.Vertex (2,10))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (20,14)) (Day_2022_15.Vertex (25,17))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (17,20)) (Day_2022_15.Vertex (21,22))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (16,7))  (Day_2022_15.Vertex (15,3))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (14,3))  (Day_2022_15.Vertex (15,3))
                               , Day_2022_15.Sensor (Day_2022_15.Vertex (20,1))  (Day_2022_15.Vertex (15,3))
                               ]
            describe "parse" . mapSubject Day_2022_15.parse $ do
                it "should produce the example input" $ \result ->
                    result `shouldBe` Right exampleInput
            describe "main" . mapSubject (Day_2022_15.main 20) $ do
                it "should produce the expected result" $ \result ->
                    result `shouldBe` Right (Just 56000011)

    describe "2022-15" $ do
        describe "with example input file" . before (getDataFileName "2022-16-example.txt" >>= readFile) $ do
            let exampleInput = [ Day_2022_16.Valve "AA"  0 ["DD", "II", "BB"]
                               , Day_2022_16.Valve "BB" 13 ["CC", "AA"]
                               , Day_2022_16.Valve "CC"  2 ["DD", "BB"]
                               , Day_2022_16.Valve "DD" 20 ["CC", "AA", "EE"]
                               , Day_2022_16.Valve "EE"  3 ["FF", "DD"]
                               , Day_2022_16.Valve "FF"  0 ["EE", "GG"]
                               , Day_2022_16.Valve "GG"  0 ["FF", "HH"]
                               , Day_2022_16.Valve "HH" 22 ["GG"]
                               , Day_2022_16.Valve "II"  0 ["AA", "JJ"]
                               , Day_2022_16.Valve "JJ" 21 ["II"]
                               ]
            describe "parse" . mapSubject Day_2022_16.parse $ do
                it "should produce the example input" $ \result ->
                    result `shouldBe` Right exampleInput
            describe "main" . mapSubject Day_2022_16.main $ do
                it "should produce the expected result" $ \result ->
                    result `shouldBe` Right (1651, 1707)
