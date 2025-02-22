{-# LANGUAGE OverloadedStrings #-}

module CursesWrapper (initWrapper, refreshScreen, baseColor, highlightColor, availableColor, selectedColor, CellColor) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Monad (when)
import System.Exit (exitSuccess)
import Data.Char

type CellColor = Pair

baseColor :: Pair 
baseColor = Pair 1

highlightColor :: Pair 
highlightColor = Pair 2

availableColor :: Pair 
availableColor = Pair 3

selectedColor :: Pair 
selectedColor = Pair 4

initWrapper :: IO ()
initWrapper = do
  -- HSCurses flags
  setLocaleEncoding utf8
  initScr
  initCurses
  startColor
  cBreak True  
  echo False   
  keypad stdScr True   
  cursSet CursorInvisible  

  initPair baseColor white black
  initPair highlightColor white yellow
  initPair availableColor white cyan
  initPair selectedColor white cyan

refreshScreen :: Int -> Int -> [[(Char, CellColor)]] -> IO ()
refreshScreen startLine startCol matrix = do
  wclear stdScr
  refreshLoop startLine startCol startLine startCol matrix
  refresh 

refreshLoop :: Int -> Int -> Int -> Int -> [[(Char, CellColor)]] -> IO ()
refreshLoop startLine startCol line col matrix
    | line >= endLine = return ()
    | col >= endCol = refreshLoop startLine startCol (line + 1) startCol matrix
    | otherwise = do
        updatePoint startLine startCol line col matrix
        refreshLoop startLine startCol line (col + 1) matrix
  where
    endLine = startLine + (length matrix)
    endCol  = startCol + (if null matrix then 0 else length (head matrix))

updatePoint :: Int -> Int -> Int -> Int -> [[(Char, CellColor)]] -> IO ()
updatePoint startLine startCol line col matrix = do
  let (char, color) = (matrix !! (line - startLine)) !! (col - startCol)
  attrSet attr0 color
  mvAddCh line col (fromIntegral (ord char))
  attrSet attr0 baseColor