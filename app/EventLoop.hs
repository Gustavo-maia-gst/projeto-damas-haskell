{-# LANGUAGE OverloadedStrings #-}
module EventLoop  where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import GameState
import CursesWrapper (initWrapper)
import Render
import Control.Monad (when)
import Control.Lens
import System.Exit (exitSuccess)
import Plinio (handleTurn)
import Navigation
import HandleAction
import InitialScreen


init :: Int -> IO ()
init opt = do
  initWrapper
  let state = makeInitialState
  eventLoop opt state

eventLoop :: Int -> GameState -> IO ()
eventLoop opt state = do
  let stateAux = if state ^. turn == P2 && opt == 1 then handleTurn state else state

  checkEndGame stateAux opt

  refreshBoard stateAux

  key <- getCh 

  let newState = case key of
          KeyChar 'w'   ->  handleUp stateAux
          KeyUp   ->  handleUp stateAux

          KeyChar 's'   ->  handleDown stateAux
          KeyDown   ->  handleDown stateAux

          KeyChar 'a'   ->  handleLeft stateAux
          KeyLeft   ->  handleLeft stateAux

          KeyChar 'd'   ->  handleRight stateAux
          KeyRight   ->  handleRight stateAux
          
          KeyChar ' '   ->  handleAction stateAux
          KeyChar '\n'  ->  handleAction stateAux
          _             ->  stateAux  -- Se qualquer outra tecla for pressionada, não muda o estado
  

  if key == KeyChar 'q'
    then endWin >> exitSuccess
    else eventLoop opt newState

checkEndGame :: GameState -> Int -> IO ()
checkEndGame state opt
  | isBot && p1Qtd == 0  = do
    printEnd "Plinio"
  | p1Qtd == 0           = do
    printEnd "Jogador 2"
  | p2Qtd == 0           = do
    printEnd "Jogador 1"
  | otherwise              = return ()
  where
    p1Qtd   = state ^. p1Count
    p2Qtd   = state ^. p2Count
    isBot   = opt == 1

printEnd :: String -> IO ()
printEnd text = do
  wclear stdScr
  printCentered (text ++ " Ganhou!! 🥳🥳") 10
  refresh

  key <- getCh 

  endWin >> exitSuccess