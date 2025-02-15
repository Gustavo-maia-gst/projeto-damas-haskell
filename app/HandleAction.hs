module HandleAction where

import GameState
import Control.Lens
import Utils (hasSelection)

-- TODO remove after implementing handleMovement
handleMovement :: GameState -> GameState
handleMovement state = state

handleAction :: GameState -> GameState
handleAction state = 
    if isSelected then
      handleSelection state
    else
      handleMovement state
    where
        isSelected = hasSelection state