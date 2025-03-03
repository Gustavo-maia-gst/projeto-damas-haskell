{-# LANGUAGE OverloadedStrings #-}
module EventLoop  where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import GameState
import CursesWrapper (initWrapper)
import Render as R
import Control.Monad (when)
import Control.Lens
import System.Exit (exitSuccess)
import Plinio (handleTurn)
import Navigation
import HandleAction


init :: Int -> IO ()
init opt = do
  initWrapper
  let state = makeInitialState
  eventLoop opt state

eventLoop :: Int -> GameState -> IO ()
eventLoop opt state = do
  let stateAux = if state ^. turn == P2 && opt == 1 then handleTurn state else state

  R.refresh stateAux

  key <- getCh 

  let newState = case key of
          KeyChar 'w'   ->  handleUp stateAux
          KeyChar 's'   ->  handleDown stateAux
          KeyChar 'a'   ->  handleLeft stateAux
          KeyChar 'd'   ->  handleRight stateAux
          KeyChar ' '   ->  handleAction stateAux
          KeyChar '\n'  ->  handleAction stateAux
          _             ->  stateAux  -- Se qualquer outra tecla for pressionada, não muda o estado
  

  if key == KeyChar 'q'
    then endWin >> exitSuccess
    else eventLoop opt newState
  