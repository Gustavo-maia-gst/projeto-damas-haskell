{-# LANGUAGE RecordWildCards #-}
module CellState where

import GameState
import Data.Bits

data StateCell = StateCell
    {
        _cursor :: Bool,
        _possibleMoviment :: Bool,
        _turn :: Turn,
        _isDama :: Bool
    } deriving Show

bitPossibleMoviment = 0 :: Int
bitCursor = 1 :: Int
bitDama = 2 :: Int
bitP1 = 3 :: Int
bitP2 = 4 :: Int


numberTurn :: Turn -> Int
numberTurn P1 = shiftL 1 bitP1
numberTurn P2 = shiftL 1 bitP2

numberDama = shiftL 1 bitDama :: Int

numberCursor = shiftL 1 bitCursor :: Int

numberPossibleMoviment = shiftL 1 bitPossibleMoviment :: Int

getNumber :: StateCell -> Int
getNumber StateCell{..} = let
    n1 = numberTurn _turn
    n2 = if _isDama then numberDama else 0
    n3 = if _cursor then numberCursor else 0
    n4 = if _possibleMoviment then numberPossibleMoviment else 0
    numberFinal = n1 .|. n2 .|. n3 .|. n4
    in numberFinal

defaultStateCell :: StateCell
defaultStateCell = StateCell
    { _cursor = False,
      _possibleMoviment = False,
     _turn = P1,
     _isDama = False
    }
