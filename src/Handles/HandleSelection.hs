module HandleSelection where

import GameState
import Control.Lens
import Utils
import MovementFinder

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
        newState = findValidMoves stateAux False

closeSelection :: GameState -> GameState
closeSelection state 
    | state ^. isLocked == True  = state
    | otherwise                  = clearSelection state