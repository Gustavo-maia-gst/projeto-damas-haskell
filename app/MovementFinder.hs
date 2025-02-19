module MovementFinder where

import GameState
import Control.Lens
import Utils

findValidMoves :: GameState -> GameState
findValidMoves state = case state ^. selected of
    Nothing -> state
    Just (x, y) -> 
        let cell = getCell x y state
        in case cell ^. player of
            Nothing -> state
            Just player ->
                if player /= state ^. turn
                    then state
                else
                    let king = cell ^. isKing
                        direction = getMoveDirections player king
                    in foldl (\acc (dx, dy) -> checkDirection acc (dx, dy) (x, y) 0 direction) state direction

-- Movement directions based on the player type
-- Currently i'm representing A1 on the Board as (0,0) on the list
-- If this chenges, just reverse P1 and P2
getMoveDirections :: Player -> Bool -> [(Int, Int)]
getMoveDirections _ True = [(1, -1), (1, 1), (-1 , 1), (-1, -1)] -- King can move in any direction
getMoveDirections P1 _ = [(1, -1), (1, 1)]  -- Up-left and up-right for P1
getMoveDirections P2 _ = [(-1, -1), (-1, 1)] -- Down-left and down-right for P2
getMoveDirections _ _        = []  -- No valid moves if no player

checkDirection :: GameState -> (Int, Int) -> (Int, Int) -> (Int) -> [(Int, Int)] -> GameState
checkDirection state (dx, dy) (x, y) numberOfJumps direction
    | not (isInBounds newX newY) = state -- Out of bounds, return unchaged
    | isEmpty targetCell && numberOfJumps == 0 = state & matrix . ix newX . ix newY . available .~ True  -- If empty, mark as valid move
    | isEnemy targetCell player && isInBounds jumpX jumpY && isEmpty jumpTargetCell =
        let 
            newState = state & matrix . ix jumpX . ix jumpY . available .~ True -- Enemy piece in the diagonal, check if the next diagonal is free
        in 
            foldl (\acc (dx, dy) -> checkDirection acc (dx, dy) (jumpX, jumpY) (numberOfJumps + 1) direction) newState direction
    | otherwise = state -- Ally piece, invalid move
    where
        (newX, newY) = (x + dx, y + dy) -- New coordinates
        (jumpX, jumpY) = (x + 2 * dx, y + 2 * dy)
        targetCell = getCell newX newY state
        jumpTargetCell = getCell jumpX jumpY state
        player = state ^. turn
        



