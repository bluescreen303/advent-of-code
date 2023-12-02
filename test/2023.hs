{-# LANGUAGE DataKinds #-}
module Main (main) where

import Test.Hspec
import System.FilePath (joinPath)
import qualified Paths_advent_of_code (getDataFileName)
import qualified Year2023.Day01 as Day01
import qualified Year2023.Day02 as Day02
import Data.Either (isRight)
import Helpers (doParse)

getDataFileName :: FilePath -> IO FilePath
getDataFileName x = Paths_advent_of_code.getDataFileName $ joinPath ["2023", x]

main :: IO ()
main = hspec . parallel $ do

    describe "2023-01" $ do
        describe "the first puzzle" $ do
            describe "lineFirstLast" . before (return $ Day01.lineFirstLast False) $ do
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "18" `shouldBe` (1, 8)
                it "should grab the first and last digits when there is stuff around it" $ \lineFirstLast ->
                    lineFirstLast "a12a3b8q" `shouldBe` (1, 8)
                it "should take a single digit as first *and* last" $ \lineFirstLast ->
                    lineFirstLast "barfoo1foobar" `shouldBe` (1, 1)
            describe "with the example input" . before (getDataFileName "01-example.txt" >>= readFile) $ do
                describe "result" . mapSubject (Day01.main False) $ do
                    it "should be 142" $ \result ->
                        result `shouldBe` 142
        describe "the second puzzle" $ do
            describe "lineFirstLast" . before (return $ Day01.lineFirstLast True) $ do
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "two1nine" `shouldBe` (2, 9)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "eightwothree" `shouldBe` (8, 3)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "abcone2threexyz" `shouldBe` (1, 3)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "xtwone3four" `shouldBe` (2, 4)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "4nineeightseven2" `shouldBe` (4, 2)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "zoneight234" `shouldBe` (1, 4)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "7pqrstsixteen" `shouldBe` (7, 6)
                it "should grab the first and last digits" $ \lineFirstLast ->
                    lineFirstLast "oneighty" `shouldBe` (1, 8)
            describe "with the example input" . before (getDataFileName "01-example2.txt" >>= readFile) $ do
                describe "result" . mapSubject (Day01.main True) $ do
                    it "should be 281" $ \result ->
                        result `shouldBe` 281

    describe "2023-02" $ do
        describe "with the example input" . before (getDataFileName "02-example.txt" >>= readFile) $ do
            describe "the parser" . mapSubject (doParse Day02.parser) $ do
                it "should succeed" $ \result ->
                    result `shouldSatisfy` isRight
            describe "the first puzzle" $ do
                describe "result" . mapSubject (Day02.main False) $ do
                    it "should be 8" $ \result ->
                        result `shouldBe` Right 8
            describe "the second puzzle" $ do
                describe "result" . mapSubject (Day02.main True) $ do
                    it "should be 8" $ \result ->
                        result `shouldBe` Right 2286
