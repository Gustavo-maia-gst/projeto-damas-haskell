module HandleSelection where

import GameState
import Control.Lens
import Utils
-- import MovimentByFinder

checkSelection :: GameState -> Bool
checkSelection state =
        cell ^. isSelected
    where 
        cursorX = state ^. cursor ._1
        cursorY = state ^. cursor ._2
        cell = getCell cursorY cursorX state

handleSelection :: GameState -> GameState
handleSelection state 
    | not (checkSelection state) = state
    | otherwise = let
        cursorX = state ^. cursor . _1
        cursorY = state ^. cursor . _2

        number = if state ^. turn == P1 then 4 else 8
        
        stateAux = state & selected .~ Just (cursorX, cursorY)
        newState = stateAux & matrix . ix cursorX . ix cursorY . isSelected .~ False
        -- newState = movimentByFinder state2
    in
        newState