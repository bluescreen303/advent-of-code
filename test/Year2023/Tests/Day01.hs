module Year2023.Tests.Day01 where

import Test.Hspec

import Helpers
import Year2023.Day01

mainSpec :: SpecWith ()
mainSpec = do
    describe "the first puzzle" $ do
        describe "lineFirstLast" . before (return $ lineFirstLast False) $ do
            it "should grab the first and last digits" $ \self ->
                self "18" `shouldBe` (1, 8)
            it "should grab the first and last digits when there is stuff around it" $ \self ->
                self "a12a3b8q" `shouldBe` (1, 8)
            it "should take a single digit as first *and* last" $ \self ->
                self "barfoo1foobar" `shouldBe` (1, 1)
        describe "with the example input" . before (getDataFileName 2023 "01-example.txt" >>= readFile) $ do
            describe "result" . mapSubject (main False) $ do
                it "should be 142" $ \result ->
                    result `shouldBe` 142

    describe "the second puzzle" $ do
        describe "lineFirstLast" . before (return $ lineFirstLast True) $ do
            it "should grab the first and last digits" $ \self ->
                self "two1nine" `shouldBe` (2, 9)
            it "should grab the first and last digits" $ \self ->
                self "eightwothree" `shouldBe` (8, 3)
            it "should grab the first and last digits" $ \self ->
                self "abcone2threexyz" `shouldBe` (1, 3)
            it "should grab the first and last digits" $ \self ->
                self "xtwone3four" `shouldBe` (2, 4)
            it "should grab the first and last digits" $ \self ->
                self "4nineeightseven2" `shouldBe` (4, 2)
            it "should grab the first and last digits" $ \self ->
                self "zoneight234" `shouldBe` (1, 4)
            it "should grab the first and last digits" $ \self ->
                self "7pqrstsixteen" `shouldBe` (7, 6)
            it "should grab the first and last digits" $ \self ->
                self "oneighty" `shouldBe` (1, 8)
        describe "with the example input" . before (getDataFileName 2023 "01-example2.txt" >>= readFile) $ do
            describe "result" . mapSubject (main True) $ do
                it "should be 281" $ \result ->
                    result `shouldBe` 281
