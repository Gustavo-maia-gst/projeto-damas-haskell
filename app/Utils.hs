module Utils where

import Control.Lens
import GameState
import Data.Bits

hasSelection :: GameState -> Bool
hasSelection state = case state ^. selected of
  Just _  -> True
  Nothing  -> False

getCell :: Int -> Int -> GameState -> Cell
getCell line col state = 
  let 
    m = state ^. matrix 
  in
    (m !! line) !! col