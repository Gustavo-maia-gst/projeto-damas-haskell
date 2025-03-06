{-# LANGUAGE OverloadedStrings #-}

module Render (refreshBoard) where

import GameState
import Utils
import CursesWrapper
import Control.Lens
import Data.Char (toLower, toUpper)
import UI.HSCurses.Curses (stdScr, wAddStr, scrSize, move)

refreshBoard :: GameState -> IO ()
refreshBoard state = do
  let charMatrix = makeMatrixChar state

  (lines, cols) <- scrSize

  let startCol = (cols - (length (head charMatrix))) `div` 2

  clearScreen

  writeHeaders state (startCol `div` 3)

  refreshScreen 0 startCol charMatrix

writeHeaders :: GameState -> Int -> IO ()
writeHeaders state startCol = do
  move 2 startCol
  wAddStr stdScr "Turno"
  move 3 startCol
  wAddStr stdScr (if state ^. turn == P1 then "Jogador 1" else "Jogador 2")


  move 5 startCol
  wAddStr stdScr "Peças"
  move 6 startCol
  wAddStr stdScr ("● J1: " ++ (show (state ^. p1Count)))
  move 7 startCol
  wAddStr stdScr ("○ J2: " ++ (show (state ^. p2Count)))
  
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