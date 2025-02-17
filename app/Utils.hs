module Utils where

import Control.Lens
import GameState
import Data.Bits

hasSelection :: GameState -> Bool
hasSelection state = case state ^. selected of
  Just _  -> True
  Nothing  -> False

getCell :: Int -> Int -> GameState -> Cell
getCell line col state = 
  let 
    m = state ^. matrix 
  in
    (m !! line) !! col

checkBit :: Int -> Int -> Bool
checkBit bit number  = (number .&. (1 `shiftL` bit)) /= 0

isInBounds :: Int -> Int -> Bool
isInBounds x y = (x >= 0 && x < 8) && (y >= 0 && y < 8)

isEmpty :: Cell -> Bool
isEmpty cell = cell ^. player == Nothing

isEnemy :: Cell -> Player -> Bool
isEnemy cell currentPlayer = case cell ^. player of
    Just p  -> p /= currentPlayer  
    Nothing -> False     