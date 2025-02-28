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
import Navigation
import HandleAction


init :: Int -> IO ()
init opt = do
  initWrapper
  let state = makeInitialState
  eventLoop opt state

eventLoop :: Int -> GameState -> IO ()
eventLoop opt state = do
  R.refresh state
  key <- getCh 

  let newState = case key of
          KeyChar 'w'   ->  handleUp state
          KeyChar 's'   ->  handleDown state
          KeyChar 'a'   ->  handleLeft state
          KeyChar 'd'   ->  handleRight state
          KeyChar ' '   ->  handleAction state
          KeyChar '\n'  ->  handleAction state
          _             ->  state  -- Se qualquer outra tecla for pressionada, não muda o estado

  if key == KeyChar 'q'
    then endWin >> exitSuccess
    else eventLoop opt newState
  