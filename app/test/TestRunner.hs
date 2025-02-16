module Main where

import Test.HUnit
import TestSimple  

main :: IO Counts
main = do
    putStrLn "Running tests..."
    runTestTT (TestList [testAddition, testSubtraction])  
