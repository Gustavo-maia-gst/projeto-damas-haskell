module HandleSelection where

import GameState
import Control.Lens
import Utils
-- import MovimentByFinder

checkSelection :: GameState -> Bool
checkSelection state = 
    playerInCell == Just playerInTurn 
    where 
        cursorY = state ^. cursor ._1
        cursorX = state ^. cursor ._2
        cell = getCell cursorY cursorX state
        playerInCell = cell ^. player
        playerInTurn = state ^. turn

handleSelection :: GameState -> GameState
handleSelection state 
    | not (checkSelection state) = state
    | otherwise = newState
        where
        cursorY = state ^. cursor . _1
        cursorX = state ^. cursor . _2

        stateAux = state & selected .~ Just (cursorY, cursorX)
        -- acho que deve deixar ele selecionado e o cursor tem preferencia pelo selecionado
        newState = stateAux & matrix . ix cursorY . ix cursorX . isSelected .~ False
        -- newState = movimentByFinder state2