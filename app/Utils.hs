module Utils where

import Control.Lens
import GameState
import Data.Bits

hasSelection :: GameState -> Bool
hasSelection state = case state ^. selected of
  Just _  -> True
  Nothing  -> False

clearSelection :: GameState -> GameState
clearSelection state = state & selected .~ Nothing

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


--