module Year2023.Tests.Day02 where

import Test.Hspec

import Helpers
import Year2023.Day02
import Data.Either (isRight)

mainSpec :: SpecWith ()
mainSpec = do
        describe "with the example input" . before (getDataFileName 2023 "02-example.txt" >>= readFile) $ do
            describe "the parser" . mapSubject (doParse parser) $ do
                it "should succeed" $ \result ->
                    result `shouldSatisfy` isRight
            describe "the first puzzle" $ do
                describe "result" . mapSubject (main False) $ do
                    it "should be 8" $ \result ->
                        result `shouldBe` Right 8
            describe "the second puzzle" $ do
                describe "result" . mapSubject (main True) $ do
                    it "should be 8" $ \result ->
                        result `shouldBe` Right 2286
