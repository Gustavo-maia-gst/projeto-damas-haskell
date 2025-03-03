module HandleMovement where

import GameState
import Control.Lens
import Utils
import MovementFinder

destroyer :: GameState -> GameState
destroyer state 
    | dLine == 1 && dCol == 1 = state
    | otherwise = newState
    where
        (cursorLine, cursorCol) = state ^. cursor 

        Just (atLine, atCol) = state ^. selected

        dLine = abs (atLine - cursorLine)
        dCol = abs (atCol - cursorCol)
        (itmLine, itmCol) = (div (cursorLine + atLine) 2, div (cursorCol + atCol) 2)

        stateAux1 = reduceOpponentCount state
        stateAux2 = lock stateAux1
        newState  = stateAux2 & matrix . ix itmLine . ix itmCol .~ defaultCell

move :: GameState -> GameState
move state = newState 
    where
        Just (atLine, atCol) = state ^. selected
        (cursorLine, cursorCol) = state ^. cursor 
        vez = state ^. turn
        cellAnterior = getCell atLine atCol state
       
        stateAux1 = state & selected .~ Just (cursorLine, cursorCol)
        stateAux2 = stateAux1 & matrix . ix atLine . ix atCol .~ defaultCell
        stateAux3 = stateAux2 & matrix . ix cursorLine . ix cursorCol .~ cellAnterior
        newState  = 
            if cursorLine == 7 || cursorLine == 0 
            then stateAux3 & matrix . ix cursorLine . ix cursorCol . isKing .~ True
            else stateAux3
        


handleMovement :: GameState -> GameState
handleMovement state = newState
        where
            stateLimpo = unlock state
            stateAux1  = destroyer stateLimpo
            stateAux2  = move stateAux1
            stateAux3  = setAllCellsUnavailable stateAux2
            stateAux4  = findValidMoves stateAux3 True
            newState   = nxtState stateAux4


nxtState :: GameState -> GameState 
nxtState state
    | continua && capturou = lock state
    | otherwise            = newState
    where
        continua  = hasAvailableMove state
        capturou  = state ^. isLocked
        stateAux1 = state & selected .~ Nothing
        stateAux2 = changeTurn stateAux1
        stateAux3 = setAllCellsUnavailable stateAux2
        newState  = unlock stateAux3