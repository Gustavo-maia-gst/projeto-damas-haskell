module Navigation where

import GameState
import Control.Lens

isValid :: Int -> Int -> Bool
isValid x y 
    | x < 0 || x >= 8 = False
    | y < 0 || y >= 8 = False
    | otherwise = True

handleUp :: GameState -> GameState
handleUp state 
    | not (isValid  (cursorY-1) cursorX) = state
    | otherwise = newState
    where
        cursorY = state ^. cursor . _1
        cursorX = state ^. cursor . _2

        state2 = (state & matrix . ix cursorY . ix cursorX . isSelected .~ False) & cursor . _1 -~ 1
        newCursorY = state2 ^. cursor . _1
        newCursorX = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorY . ix newCursorX . isSelected .~ True

handleDown :: GameState -> GameState
handleDown state 
    | not (isValid (cursorY+1) cursorX) = state
    | otherwise = newState
    where
        cursorY = state ^. cursor . _1
        cursorX = state ^. cursor . _2

        state2 = (state & matrix . ix cursorY . ix cursorX . isSelected .~ False) & cursor . _1 +~ 1
        newCursorY = state2 ^. cursor . _1
        newCursorX = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorY . ix newCursorX . isSelected .~ True

handleRight :: GameState -> GameState
handleRight state 
    | not (isValid cursorY (cursorX + 1)) = state
    | otherwise = newState
    where
        cursorY = state ^. cursor . _1
        cursorX = state ^. cursor . _2

        state2 = (state & matrix . ix cursorY . ix cursorX . isSelected .~ False) & cursor . _2 +~ 1
        newCursorY = state2 ^. cursor . _1
        newCursorX = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorY . ix newCursorX . isSelected .~ True

handleLeft :: GameState -> GameState
handleLeft state 
    | not (isValid cursorY (cursorX - 1)) = state
    | otherwise = newState
    where
        cursorY = state ^. cursor . _1
        cursorX = state ^. cursor . _2

        state2 = (state & matrix . ix cursorY . ix cursorX . isSelected .~ False) & cursor . _2 -~ 1
        newCursorY = state2 ^. cursor . _1
        newCursorX = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorY . ix newCursorX . isSelected .~ True

    