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
    | not (isValid  (cursorLine-1) cursorCol) = state
    | otherwise = newState
    where
        cursorLine = state ^. cursor . _1
        cursorCol = state ^. cursor . _2

        state2 = (state & matrix . ix cursorLine . ix cursorCol . isUnderCursor .~ False) & cursor . _1 -~ 1
        newCursorLine = state2 ^. cursor . _1
        newCursorCol = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorLine . ix newCursorCol . isUnderCursor .~ True

handleDown :: GameState -> GameState
handleDown state 
    | not (isValid (cursorLine+1) cursorCol) = state
    | otherwise = newState
    where
        cursorLine = state ^. cursor . _1
        cursorCol = state ^. cursor . _2

        state2 = (state & matrix . ix cursorLine . ix cursorCol . isUnderCursor .~ False) & cursor . _1 +~ 1
        newCursorLine = state2 ^. cursor . _1
        newCursorCol = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorLine . ix newCursorCol . isUnderCursor .~ True

handleRight :: GameState -> GameState
handleRight state 
    | not (isValid cursorLine (cursorCol + 1)) = state
    | otherwise = newState
    where
        cursorLine = state ^. cursor . _1
        cursorCol = state ^. cursor . _2

        state2 = (state & matrix . ix cursorLine . ix cursorCol . isUnderCursor .~ False) & cursor . _2 +~ 1
        newCursorLine = state2 ^. cursor . _1
        newCursorCol = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorLine . ix newCursorCol . isUnderCursor .~ True

handleLeft :: GameState -> GameState
handleLeft state 
    | not (isValid cursorLine (cursorCol - 1)) = state
    | otherwise = newState
    where
        cursorLine = state ^. cursor . _1
        cursorCol = state ^. cursor . _2

        state2 = (state & matrix . ix cursorLine . ix cursorCol . isUnderCursor .~ False) & cursor . _2 -~ 1
        newCursorLine = state2 ^. cursor . _1
        newCursorCol = state2 ^. cursor . _2
        newState = state2 & matrix . ix newCursorLine . ix newCursorCol . isUnderCursor .~ True

    