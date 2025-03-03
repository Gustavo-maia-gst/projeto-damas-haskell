module Plinio (handleTurn) where

import GameState
import Control.Lens
import Utils
import HandleMovement

handleTurn :: GameState -> GameState
handleTurn state = newState
  where
    updatedState = fst (explore state)
    newState     = updatedState & turn .~ P1

explore :: GameState -> (GameState, Int)
explore state = foldl 
                (\(bestState, bestScore) (i, j) -> 
                  if snd (getBest state i j) > bestScore then getBest state i j
                  else (bestState, bestScore)
                ) 
                (state, worstScore)
                possibleCells
  where
    possibleCells = filter (\(i, j) -> (getCell i j state) ^. player == Just P2) [(i, j) | i <- [0 .. 7], j <- [0 .. 7]]

getBest :: GameState -> Int -> Int -> (GameState, Int)
getBest state i j
    | length moves == 0   = (stateWithSelected, worstScore)
    | otherwise           = getBestLoop stateWithSelected 0 moves
  where
    stateWithSelected = state & selected .~ Just (i, j)
    moves = possibleMoves stateWithSelected i j

getBestLoop :: GameState -> Int -> [(Int, Int, Bool)] -> (GameState, Int)
getBestLoop state i moves
    | i == movesSize  = (state, worstScore)
    | otherwise       = if moveStateScore > loopStateScore then 
                          (moveState, moveStateScore)
                        else
                          (loopState, loopStateScore)
  where
    movesSize                   = length moves
    move                        = moves !! i
    moveState                   = makeMove state move
    moveStateScore              = getScore moveState
    (loopState, loopStateScore) = getBestLoop state (i + 1) moves

makeMove :: GameState -> (Int, Int, Bool) -> GameState
makeMove state (i, j, eating)
    | not eating  = moveWrapper stateWithCursor
    | otherwise   = tryKeepEating (moveWrapper stateWithCursor)
  where
    stateWithCursor = state & cursor .~ (i, j)

moveWrapper :: GameState -> GameState
moveWrapper state = unlock newState
  where
    cleanState  = unlock state
    stateAux0   = destroyer cleanState
    stateAux1   = move stateAux0
    stateAux2   = setAllCellsUnavailable stateAux1
    newState    = stateAux2 & selected .~ Nothing

tryKeepEating :: GameState -> GameState
tryKeepEating state
    | length eatingMoves == 0   = state
    | otherwise                 = makeMove stateAux (eatingMoves !! 0)
  where
    (i, j)            = state ^. cursor
    moves             = possibleMoves state i j
    eatingMoves       = filter (\(_, _, eating) -> eating) moves
    (newI, newJ, _)   = eatingMoves !! 0
    stateAux0         = state & selected .~ Just (i, j)
    stateAux          = stateAux0 & cursor .~ (newI, newJ)

possibleMoves :: GameState -> Int -> Int -> [(Int, Int, Bool)]
possibleMoves state i j
  | cell ^. player /= Nothing = foldl (\acc (dy, dx) -> acc ++ (directionMove state i j dy dx)) [] (getMoveDirections P2 (cell ^. isKing))
  | otherwise                 = []
  where
    cell = getCell i j state

directionMove :: GameState -> Int -> Int -> Int -> Int -> [(Int, Int, Bool)]
directionMove state i j di dj
    | (isInBounds stepI stepJ) && isEmpty (getCell stepI stepJ state)       = [(stepI, stepJ, False)]
    | (isInBounds stepI stepJ) &&
      (isInBounds jumpI jumpJ) &&
      isEnemy (getCell stepI stepJ state) P2 && isEmpty (getCell jumpI jumpJ state)  = [(jumpI, jumpJ, True)]
    | otherwise                                                       = []
  where
    (stepI, stepJ) = (i + di, j + dj)
    (jumpI, jumpJ) = (i + 2 * di, j + 2 * dj)

getScore :: GameState -> Int
getScore state = ((state ^. p2Count) - (state ^. p1Count)) * advantageMultiplier - (unprotectedPenalty * (getUnprotectedVal state))

getUnprotectedVal :: GameState -> Int
getUnprotectedVal state = sum [unprotectedVal state i j | j <- [1 .. 6], i <- [1 .. 6]]

unprotectedVal :: GameState -> Int -> Int -> Int
unprotectedVal state i j = pen1 + pen2 + pen3 + pen4
  where
    (ei1, ej1)  = (i + 1, j - 1)
    (ei2, ej2)  = (i + 1, j + 1)
    (i1, j1)    = (i - 1, j - 1)
    (i2, j2)    = (i - 1, j + 1)
    isVuln      = isCellVuln state i j
    pen1        = if isVuln && isEnemy (getCell ei1 ej1 state) P2 then 3 else 0
    pen2        = if isVuln && isEnemy (getCell ei2 ej2 state) P2 then 3 else 0
    pen3        = if isVuln then 1 else 0
    pen4        = if isVuln && hasDoubleKill state i j then 8 else 0

hasDoubleKill :: GameState -> Int -> Int -> Bool
hasDoubleKill state i j
  | not cellInBounds                      = False
  | i < 2                                 = False
  | not cellVuln                          = False
  | not leftIsEnemy && not rightIsEnemy   = False
  | leftIsEnemy && rightIsVuln            = True
  | rightIsEnemy && leftIsVuln            = True
  | otherwise                             = False
  where
    cellInBounds    = isInBounds i j
    cellVuln        = isCellVuln state i j
    leftIsVuln      = isCellVuln state (i - 2) (j - 2)
    rightIsVuln     = isCellVuln state (i - 2) (j + 2)
    leftIsEnemy     = isInBounds (i + 1) (j - 1) && isEnemy (getCell (i + 1) (j - 1) state) P2
    rightIsEnemy    = isInBounds (i + 1) (j + 1) && isEnemy (getCell (i + 1) (j + 1) state) P2

isCellVuln :: GameState -> Int -> Int -> Bool
isCellVuln state i j
    | not cellInBounds                      = False
    | cell ^. player /= Just P2             = False
    | not leftInBounds && not rightInBounds = False
    | not leftInBounds                      = rigthIsEmpty
    | not rightInBounds                     = leftIsEmpty
    | otherwise                             = rigthIsEmpty || leftIsEmpty
  where
    cellInBounds  = isInBounds i j
    cell          = getCell i j state
    leftInBounds  = isInBounds (i - 1) (j - 1)
    rightInBounds = isInBounds (i - 1) (j + 1)
    leftIsEmpty   = isEmpty (getCell (i - 1) (j - 1) state)
    rigthIsEmpty  = isEmpty (getCell (i - 1) (j + 1) state)

unprotectedPenalty :: Int
unprotectedPenalty = 1

advantageMultiplier :: Int
advantageMultiplier = 15

worstScore :: Int
worstScore = -99999