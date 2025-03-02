module HandleAction where

import GameState
import Control.Lens
import Utils (hasSelection, clearSelection)
import HandleSelection (handleSelection)
import HandleMovement

handleAction :: GameState -> GameState
handleAction state 
    | havingSelection && inAvailable     = handleMovement state
    | havingSelection && not inAvailable = clearSelection state
    | otherwise                          = handleSelection state
  where
    line            = state ^. cursor ^. _1
    col             = state ^. cursor ^. _2
    havingSelection = state ^. selected /= Nothing
    inAvailable     = (state ^. matrix . ix line) !! col ^. isAvailable