module HandleSelection where

import GameState
import CellState
import Control.Lens
import Utils
-- import MovimentByFinder

checkSelection :: GameState -> Bool
checkSelection state
    | state ^. turn == P1 && checkBit bitP1 posMatrix == True = True
    | state ^. turn == P2 && checkBit bitP2 posMatrix == True = True
    | otherwise = False
    where 
        cursorX = state ^. cursor ._1
        cursorY = state ^. cursor ._2
        posMatrix = (state ^. matrix) !! cursorX !! cursorY


handleSelection :: GameState -> GameState
handleSelection state 
    | not (checkSelection state) = state
    | otherwise = let
        cursorX = state ^. cursor . _1
        cursorY = state ^. cursor . _2

        
        newState = state & selected .~ (cursorX, cursorY, True)

        -- newState = movimentByFinder state2
    in
        newState