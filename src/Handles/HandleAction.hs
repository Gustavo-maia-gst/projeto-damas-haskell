module HandleAction where

import GameState
import Control.Lens
import Utils (hasSelection, clearSelection)
import HandleSelection (handleSelection, closeSelection)
import HandleMovement

handleAction :: GameState -> GameState
handleAction state 
    | locked && inAvailable                    = handleMovement state
    | locked                                   = state
    | havingSelection && inAvailable           = handleMovement state
    | havingSelection && not inAvailable       = closeSelection state
    | otherwise                                = handleSelection state
  where
    locked          = state ^. isLocked
    line            = state ^. cursor ^. _1
    col             = state ^. cursor ^. _2
    havingSelection = state ^. selected /= Nothing
    inAvailable     = (state ^. matrix . ix line) !! col ^. isAvailable