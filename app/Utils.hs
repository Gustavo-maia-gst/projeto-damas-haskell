module Utils where

import Control.Lens
import GameState
import Data.Bits


checkBit :: Int -> Int -> Bool
checkBit bit number  = (number .&. (1 `shiftL` bit)) /= 0

changeNumberInMatrix :: Int -> Int -> Int -> GameState -> GameState
changeNumberInMatrix x y newValue state =
    state & matrix . ix x . ix y .~ newValue