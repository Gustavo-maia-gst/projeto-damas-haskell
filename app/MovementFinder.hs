module MovementFinder where

import GameState
import Control.Lens
import Utils

-- TODO: King movements are not implemented yet.
-- TODO: Check for multiple captures

-- For now, it only contains the logic to handle basic moves like going to an empty cell
-- making a single diagonal capture, and checking if it's allowed to move to a cell based
-- on the bounds and availability.

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
                    in foldl (\acc (dx, dy) -> checkDirection acc (dx, dy) (x, y)) state direction

-- Movement directions based on the player type
getMoveDirections :: Player -> Bool -> [(Int, Int)]
getMoveDirections _ True = [(1, -1), (1, 1), (-1 , 1), (-1, -1)] -- King can move in any direction
getMoveDirections P1 _ = [(1, -1), (1, 1)]  -- Up-left and up-right for P1
getMoveDirections P2 _ = [(-1, -1), (-1, 1)] -- Down-left and down-right for P2
getMoveDirections _ _        = []  -- No valid moves if no player

checkDirection :: GameState -> (Int, Int) -> (Int, Int) -> GameState
checkDirection state (x, y) (dx, dy) 
    | not (isInBounds newX newY) = state -- Out of bounds, return unchaged
    | isEmpty targetCell = state & matrix . ix newX . ix newY . available .~ True  -- If empty, mark as valid move
    | isEnemy targetCell player = checkJump state (newX + x, newY + y) -- Enemy piece in the diagonal, check if the next diagonal is free
    | otherwise = state -- Ally piece, invalid move
    where
        (newX, newY) = (x + dx, y + dy) -- New coordinates
        targetCell = getCell newX newY state
        player = state ^. turn
        

checkJump :: GameState -> (Int, Int) -> GameState
checkJump state (jumpX, jumpY)
    | not (isInBounds jumpX jumpY) = state  -- If out of bounds, return the original state
    | isEmpty (getCell jumpX jumpY state) = state & matrix . ix jumpX . ix jumpY . available .~ True  -- If cell is empty, mark as valid move
    | otherwise = state  -- If none of the above, return the original state
    
