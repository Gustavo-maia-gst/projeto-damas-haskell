{-# LANGUAGE OverloadedStrings #-}

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import CursesWrapper (initWrapper)
import Render (refresh)
import Control.Monad (when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  initWrapper
  eventLoop             

eventLoop :: GameState -> IO ()
eventLoop state = do
  refresh state ^. matrix
  key <- getCh 

  let (ny, nx) = case key of
                   KeyChar 'w' -> (max 0 (y - 1), x)
                   KeyChar 's' -> (y + 1, x)
                   KeyChar 'a' -> (y, max 0 (x - 1))
                   KeyChar 'd' -> (y, x + 1)
                   _           -> (y, x)

  if key == KeyChar 'q'
    then endWin >> exitSuccess
    else eventLoop state