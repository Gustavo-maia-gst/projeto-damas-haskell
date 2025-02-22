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

getCell :: Int -> Int -> GameState -> Cell
getCell line col state = cell
  where
    cellSelected    = state ^. selected == Just (line, col)
    cellUnderCursor = state ^. cursor == (line, col)
    mat             = state ^. matrix
    baseCell        = (mat !! line) !! col
    cell            = (baseCell & isSelected .~ cellSelected) & isUnderCursor .~ cellUnderCursor