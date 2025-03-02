module MovementFinder where

import GameState
import Control.Lens
import Utils

-- Scans the board based on the selected piece's position and marks all possible moves 
-- by setting the target cells as `isAvailable`. 
-- If `isMultiJump` is True, only capture moves are marked.

findValidMoves :: GameState -> Bool -> GameState
findValidMoves state isMultiJump = case state ^. selected of
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
                    in foldl (\acc (dx, dy) -> checkDirection acc (dx, dy) (x, y) isMultiJump direction) state direction



getMoveDirections :: Player -> Bool -> [(Int, Int)]
getMoveDirections _ True = [(1, -1), (1, 1), (-1 , 1), (-1, -1)] -- King can move in any direction
getMoveDirections P2 _ = [(1, -1), (1, 1)]  -- Up-left and up-right for P1
getMoveDirections P1 _ = [(-1, -1), (-1, 1)] -- Down-left and down-right for P2
getMoveDirections _ _        = []  -- No valid moves if no player

checkDirection :: GameState -> (Int, Int) -> (Int, Int) -> Bool -> [(Int, Int)] -> GameState
checkDirection state (dx, dy) (x, y) isMultiJump direction
    | not (isInBounds newX newY) = state -- Out of bounds, return unchaged
    | isEmpty targetCell && isMultiJump == False = state & matrix . ix newX . ix newY . isAvailable .~ True  -- If empty, mark as valid move
    | isEnemy targetCell player && isInBounds jumpX jumpY && isEmpty jumpTargetCell = -- if the diagonal has an enemy piece, check if the next cell is empty
            state & matrix . ix jumpX . ix jumpY . isAvailable .~ True -- Mark it as available
    | otherwise = state -- Ally piece, invalid move
    where
        (newX, newY) = (x + dx, y + dy) 
        (jumpX, jumpY) = (x + 2 * dx, y + 2 * dy)
        targetCell = getCell newX newY state
        jumpTargetCell = getCell jumpX jumpY state
        player = state ^. turn
        



