module Utils where

import Control.Lens
import GameState
import Data.Bits
import Data.List

hasSelection :: GameState -> Bool
hasSelection state = case state ^. selected of
  Just _  -> True
  Nothing  -> False

clearSelection :: GameState -> GameState
clearSelection state = stateAux & selected .~ Nothing
  where 
    stateAux = setAllCellsUnavailable state

getCell line col state = cell
  where
    cellSelected    = state ^. selected == Just (line, col)
    cellUnderCursor = state ^. cursor == (line, col)
    mat             = state ^. matrix
    baseCell        = (mat !! line) !! col
    cell            = (baseCell & isSelected .~ cellSelected) & isUnderCursor .~ cellUnderCursor


isInBounds :: Int -> Int -> Bool
isInBounds x y = (x >= 0 && x < 8) && (y >= 0 && y < 8)

isEmpty :: Cell -> Bool
isEmpty cell = cell ^. player == Nothing

isEnemy :: Cell -> Player -> Bool
isEnemy cell currentPlayer = case cell ^. player of
    Just p  -> p /= currentPlayer  
    Nothing -> False     

getMoveDirections :: Player -> Bool -> [(Int, Int)]
getMoveDirections _ True = [(1, -1), (1, 1), (-1 , 1), (-1, -1)] -- King can move in any direction
getMoveDirections P2 _ = [(1, -1), (1, 1)]  -- Up-left and up-right for P1
getMoveDirections P1 _ = [(-1, -1), (-1, 1)] -- Down-left and down-right for P2
getMoveDirections _ _        = []  -- No valid moves if no player

changeTurn :: GameState -> GameState
changeTurn state = newState
  where
    vez = state ^. turn
    newState = if vez == P1 
               then state & turn .~ P2
               else state & turn .~ P1

setAllCellsUnavailable :: GameState -> GameState
setAllCellsUnavailable state =
  state & matrix . each . each . isAvailable .~ False

reduceOpponentCount gameState =
  case gameState ^. turn of
    P1 -> gameState & p2Count %~ subtract 1  
    P2 -> gameState & p1Count %~ subtract 1

-- Returns True if there is atleast one available cell
hasAvailableMove :: GameState -> Bool
hasAvailableMove state = any (any (^. isAvailable)) (state ^.matrix)

lock :: GameState -> GameState
lock state = state & isLocked .~ True

unlock :: GameState -> GameState
unlock state = state & isLocked .~ False