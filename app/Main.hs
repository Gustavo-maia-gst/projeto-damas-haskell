{-# LANGUAGE OverloadedStrings #-}

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

main :: IO ()
main = do
  initWrapper
  let state = makeInitialState
  eventLoop state

eventLoop :: GameState -> IO ()
eventLoop state = do
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
    else eventLoop newState
  