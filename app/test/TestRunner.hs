module Main where

import Test.HUnit
import TestSimple  
import TestMovementFinder
import Utils

main :: IO Counts
main = do
    putStrLn "Running tests..."
    runTestTT (TestList [testP1Moves,
     testP2Moves, testP1MovesKing, testP1MovesKing,
     testIsEnemy1, testIsEnemy2,
      testFindValidMoves1, testFindValidMoves2, testFindValidMoves3,
      testFindValidMoves4, testFindValidMoves5, testFindValidMoves6,
       testFindValidMoves7, testFindValidMoves8, testFindValidMoves9])  
