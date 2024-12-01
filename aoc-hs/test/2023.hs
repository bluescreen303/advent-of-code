module Main (main) where

import Test.Hspec
import qualified Year2023.Tests.Day01 as Day01
import qualified Year2023.Tests.Day02 as Day02

main :: IO ()
main = hspec . parallel $ do
    describe "2023-01" Day01.mainSpec
    describe "2023-02" Day02.mainSpec
