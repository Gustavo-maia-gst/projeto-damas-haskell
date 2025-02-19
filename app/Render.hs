{-# LANGUAGE OverloadedStrings #-}

module Render (refresh) where

import GameState
import Utils
import CursesWrapper
import Control.Lens
import UI.HSCurses.Curses (scrSize)

refresh :: GameState -> IO ()
refresh state = do
  let charMatrix = makeMatrixChar state

  (lines, cols) <- scrSize

  let startCol = (cols - (length (head charMatrix))) `div` 2

  refreshScreen 0 startCol charMatrix
  
makeMatrixChar :: GameState -> [[(Char, CellColor)]]
makeMatrixChar state = 
    [
      [
        makeChar i j state | j <- [0 .. endCol]
      ] | i <- [0 .. endLine]
    ]
  where
    mat         = state ^. matrix
    endLine     = (cellHeigth + 1) * (length mat)
    endCol      = (cellWidth + 1) * (length (head mat))
    

makeChar :: Int -> Int -> GameState -> (Char, CellColor)
makeChar i j state
  -- | i == 0 && j == 0                  = ('┌', baseColor)

  -- | i == 0 && j == endCol             = ('┐', baseColor)

  -- | i == endLine && j == endCol        = ('┘', baseColor)

  -- | i == endLine && j == 0            = ('└', baseColor)

  -- | i == 0 && heigthMod == 0          = ('┬', baseColor)

  -- | i == endLine && heigthMod == 0    = ('┴', baseColor)

  -- | j == 0 && lineMod == 0            = ('├', baseColor)

  -- | j == endCol && lineMod == 0       = ('┤', baseColor)

  -- | lineMod == 0 && heigthMod == 0    = ('┼', baseColor)

  -- | lineMod == 0                      = ('─', baseColor)

  -- | heigthMod == 0                    = ('│', baseColor)

  | i == 0 && j == 0                  = ('|', baseColor)

  | i == 0 && j == endCol             = ('|', baseColor)

  | i == endLine && j == endCol        = ('|', baseColor)

  | i == endLine && j == 0            = ('|', baseColor)

  | i == 0 && heigthMod == 0          = ('|', baseColor)

  | i == endLine && heigthMod == 0    = ('|', baseColor)

  | j == 0 && lineMod == 0            = ('|', baseColor)

  | j == endCol && lineMod == 0       = ('|', baseColor)

  | lineMod == 0 && heigthMod == 0    = ('|', baseColor)

  | lineMod == 0                      = ('-', baseColor)

  | heigthMod == 0                    = ('|', baseColor)

  | otherwise                         = getCellChar (getCell (i `div` (cellHeigth + 1)) (j `div` (cellWidth + 1)) state)
  where
    mat         = state ^. matrix
    lineMod     = i `mod` (cellHeigth + 1)
    heigthMod   = j `mod` (cellWidth + 1)
    endLine     = (cellHeigth + 1) * (length mat) + 1
    endCol      = (cellWidth + 1) * (length (head mat)) + 1
  
getCellChar :: Cell -> (Char, CellColor)
getCellChar cell = (char, color)
  where
      char = if cell ^. player == Just P1 then 'A' 
             else if cell ^. player == Just P2 then 'B'
             else ' '

      color = if cell ^. isSelected then highlightColor
              else if cell ^. available then availableColor
              else baseColor

cellHeigth :: Int
cellHeigth = 1

cellWidth :: Int
cellWidth = 3