module Main where

import Test.HUnit
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
       testFindValidMoves7, testFindValidMoves8, testFindValidMoves9,
       testFindValidMoves10, testFindValidMoves11, testFindValidMoves12,
       testFindValidMoves13, testFindValidMoves14, testFindValidMoves15, testFindValidMoves16,
       testHasAvailable1, testHasAvailable2])  

-- 'cabal test' to execute