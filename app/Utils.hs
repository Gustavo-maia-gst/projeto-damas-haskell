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
  let matrix = state ^. matrix
  (matrix !! line) !! col

checkBit :: Int -> Int -> Bool
checkBit bit number  = (number .&. (1 `shiftL` bit)) /= 0

changeNumberInMatrix :: Int -> Int -> Int -> GameState -> GameState
changeNumberInMatrix x y newValue state =
  state & matrix . ix x . ix y .~ newValue