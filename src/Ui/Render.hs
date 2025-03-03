{-# LANGUAGE OverloadedStrings #-}

module Render (refresh) where

import GameState
import Utils
import CursesWrapper
import Control.Lens
import Data.Char (toLower, toUpper)
import UI.HSCurses.Curses (stdScr, wAddStr, scrSize, move)

refresh :: GameState -> IO ()
refresh state = do
  let charMatrix = makeMatrixChar state

  (lines, cols) <- scrSize

  let startCol = (cols - (length (head charMatrix))) `div` 2

  clearScreen

  move 2 25
  wAddStr stdScr (if state ^. turn == P1 then "Jogador 1" else "Jogador 2")
  move 3 25
  wAddStr stdScr ((show (state ^. p1Count)) ++ "x" ++ (show (state ^. p2Count)))

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
  | i == 0 && j == 0                  = ('┌', baseColor)

  | i == 0 && j == endCol             = ('┐', baseColor)

  | i == endLine && j == endCol        = ('┘', baseColor)

  | i == endLine && j == 0            = ('└', baseColor)

  | i == 0 && heigthMod == 0          = ('┬', baseColor)

  | i == endLine && heigthMod == 0    = ('┴', baseColor)

  | j == 0 && lineMod == 0            = ('├', baseColor)

  | j == endCol && lineMod == 0       = ('┤', baseColor)

  | lineMod == 0 && heigthMod == 0    = ('┼', baseColor)

  | lineMod == 0                      = ('─', baseColor)

  | heigthMod == 0                    = ('│', baseColor)

  | otherwise                         = getCellChar (getCell cellLine cellCol state) (heigthMod == 2)
  where
    mat         = state ^. matrix
    lineMod     = i `mod` (cellHeigth + 1)
    heigthMod   = j `mod` (cellWidth + 1)
    endLine     = (cellHeigth + 1) * (length mat)
    endCol      = (cellWidth + 1) * (length (head mat))
    cellLine    = i `div` (cellHeigth + 1)
    cellCol     = j `div` (cellWidth + 1)
  
getCellChar :: Cell -> Bool -> (Char, CellColor)
getCellChar cell isMiddle = (char, color)
  where
      playerChar = if not isMiddle || cell ^. player == Nothing then ' '
                   else if cell ^. player == Just P1 then '●' 
                   else '○'
      
      char = if cell ^. isKing then
             if cell ^. player == Just P1 then '◉'
             else '◍'
             else playerChar 

      color = if cell ^. isUnderCursor then highlightColor
              else if cell ^. isSelected then selectedColor
              else if cell ^. isAvailable then availableColor
              else baseColor

cellHeigth :: Int
cellHeigth = 1

cellWidth :: Int
cellWidth = 3