module Main (main) where

import Test.Hspec
import Paths_advent_of_code (getDataFileName)
import qualified Day_2022_01
import qualified Day_2022_02
import Day_2022_02 (OtherSide(..), OurSide(..))
import qualified Day_2022_03
import qualified Day_2022_04

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
