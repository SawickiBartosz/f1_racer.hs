module Main where

import Test.HUnit
import Physics

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2] where
    test1 = TestCase (assertEqual "Is 3 == 3" 3 3)
    test2 = TestCase (assertEqual "Is 3 == 3 * 1" 3 (1*3))

main :: IO()
main = runTestTTAndExit tests
