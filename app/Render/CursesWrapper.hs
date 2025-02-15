module CursesWrapper (initWrapper, refreshScreen) where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Monad (when)
import System.Exit (exitSuccess)

type Color = Int

initWrapper :: IO ()
initWrapper = do
  -- HSCurses flags
  initCurses
  cBreak True  
  echo False   
  keypad stdScr True   
  cursSet CursorInvisible  

refreshScreen :: Int -> Int -> [[(Char, Color)]] -> IO ()
refreshScreen startLine startCol matrix = do
  wclear stdScr
  refreshCont startLine startCol startLine startCol matrix
  refresh 

refreshLoop :: Int -> Int -> Int -> Int -> [[(Char, Color)]] -> IO ()
refreshLoop startLine startCol line col matrix
    | line >= endLine = return ()
    | col >= endCol = refreshLoop startLine startCol (line + 1) startCol matrix
    | otherwise = do
        updatePoint line col matrix
        refreshLoop startLine startCol line (col + 1) matrix
  where
    endLine = startLine + (length matrix)
    endCol  = startCol + (if null matrix then 0 else length (head matrix))

updatePoint :: Int -> Int -> [[(Char, Color)]] -> IO ()
updatePoint line col matrix = do
  let (char, color) = (matrix !! line) !! col
  attron (ColorPair color)
  mvAddCh line col char
  attroff (ColorPair color)

BASE_COLOR :: Int 
BASE_COLOR = 1

HIGHLIGTH_COLOR :: Int 
HIGHLIGTH_COLOR = 2

SELECTED_COLOR :: Int 
SELECTED_COLOR = 3

AVAILABLE_COLOR :: Int 
AVAILABLE_COLOR = 4