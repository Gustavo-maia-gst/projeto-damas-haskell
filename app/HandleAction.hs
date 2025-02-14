module HandleAction where

import GameState
import Control.Lens

handleAction :: GameState -> GameState
handleAction state 
    | (state ^. selected ^. _3) == False = handleSelection state
    -- | (state ^. selected ^. _3) == True = handleMovement state

