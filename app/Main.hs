{-# LANGUAGE OverloadedStrings #-}

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import GameState
import CursesWrapper (initWrapper)
import Render as R
import Control.Monad (when)
import Control.Lens
import System.Exit (exitSuccess)

main :: IO ()
main = do
  initWrapper
  let state = makeInitialState
  eventLoop state

eventLoop :: GameState -> IO ()
eventLoop state = do
  R.refresh state
  key <- getCh 

  case key of
    KeyChar 'w' -> R.refresh state 
    KeyChar 's' -> R.refresh state 
    KeyChar 'a' -> R.refresh state
    KeyChar 'd' -> R.refresh state 
    KeyChar 'q' -> endWin >> exitSuccess
    _           -> R.refresh state