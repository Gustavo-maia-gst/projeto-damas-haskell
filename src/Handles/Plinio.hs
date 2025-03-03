module Plinio (handleTurn) where

import GameState
import Control.Lens
import Utils
import HandleMovement (move, destroyer)

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
                (state, 0)
                possibleCells
  where
    possibleCells = filter (\(i, j) -> (getCell i j state) ^. player == Just P2) [(i, j) | i <- [0 .. 7], j <- [0 .. 7]]

getBest :: GameState -> Int -> Int -> (GameState, Int)
getBest state i j
    | length moves == 0   = (stateWithSelected, 0)
    | otherwise           = getBestLoop stateWithSelected 0 moves
  where
    stateWithSelected = state & selected .~ Just (i, j)
    moves = possibleMoves stateWithSelected i j

getBestLoop :: GameState -> Int -> [(Int, Int, Bool)] -> (GameState, Int)
getBestLoop state i moves
    | i == movesSize  = (state, 0)
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
moveWrapper state = newState
  where
    stateAux0 = destroyer state
    newState = move stateAux0

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
getScore state = ((state ^. p2Count) - (state ^. p1Count)) * advantageMultiplier - unprotectedPenalty * (state ^. p2Count - (getUnprotectedQtd state))

getUnprotectedQtd :: GameState -> Int
getUnprotectedQtd state = length (filter (\v -> v) [isUnprotected state i j | j <- [1 .. 6], i <- [1 .. 6]])

isUnprotected :: GameState -> Int -> Int -> Bool
isUnprotected state i j = 
    if state ^. turn == P2 then
      (isEmpty (getCell (i - 1) (j + 1) state) || isEmpty (getCell (i - 1) (j - 1) state)) 
    else 
      False
  where
    mat      = state ^. matrix
    cell     = getCell i j state 

unprotectedPenalty :: Int
unprotectedPenalty = 1

advantageMultiplier :: Int
advantageMultiplier = 8