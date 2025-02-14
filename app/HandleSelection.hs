module HandleSelection where

import GameState
import Control.Lens
import Utils
-- import MovimentByFinder

checkSelection :: GameState -> Bool
checkSelection state
    | state ^. turn == P1 && checkBit 2 posMatrix == True = True
    | state ^. turn == P2 && checkBit 3 posMatrix == True = True
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

        number = if state ^. turn == P1 then 4 else 8
        
        stateAux = state & selected .~ (cursorX, cursorY, True)
        newState = changeNumberInMatrix cursorX cursorY (number + 1) stateAux
        -- newState = movimentByFinder state2
    in
        newState