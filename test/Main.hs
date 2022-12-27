module Main (main) where

import Test.Hspec
import Paths_advent_of_code (getDataFileName)
import qualified Day_2022_01 as Lib_2022_01

main :: IO ()
main = hspec . parallel $ do

    describe "2022-01" . before (getDataFileName "2022-01-example.txt" >>= readFile) $ do
        describe "the main example" . mapSubject Lib_2022_01.main $ do
            it "elf should be 4" $ \(elf, _) -> do
                elf + 1 `shouldBe` 4

            it "calories should be 24000" $ \(_, calories) -> do
                calories `shouldBe` 24000

        describe "the secondary puzzle" . mapSubject Lib_2022_01.main2 $ do
            it "calories should be 24000" $ \calories -> do
                calories `shouldBe` 45000
