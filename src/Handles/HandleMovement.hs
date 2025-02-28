module HandleMovement where

import GameState
import Control.Lens
import Utils
import MovementFinder

--destroyerR : recursivo para múltiplos mov

destroyer :: GameState -> GameState
destroyer state = newState
    where
        (cursorX, cursorY) = state ^. cursor 
        --
        Just (atX, atY) = state ^. selected
        --
        itmX = div (cursorX + atX) 2
        itmY = div (cursorY + atY) 2
        --
        stateAux = reduceOpponentCount state
        newState = stateAux & matrix . ix itmX . ix itmY . player .~ Nothing

move :: GameState -> GameState
move state = newState 
    where
        Just (atX, atY) = state ^. selected
        (cursorX, cursorY) = state ^. cursor 
        vez = state ^. turn
        --
        stateAux1 = state & selected .~ Nothing
        stateAux2 = stateAux1 & matrix . ix atX . ix atY . player .~ Nothing
        newState = stateAux2 & matrix . ix cursorX . ix cursorY . player .~ Just vez


handleMovement :: GameState -> GameState
handleMovement state = newState
        where
            -- checagem = checkMovement state
            stateAux1 = destroyer state
            stateAux2 = move stateAux1
            stateAux3 = setAllCellsUnavailable stateAux2
            newState = changeTurn stateAux3