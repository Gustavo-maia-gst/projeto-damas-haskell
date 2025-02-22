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

        newState = state & selected .~ Just (cursorY, cursorX)
        -- newState = movimentByFinder state2