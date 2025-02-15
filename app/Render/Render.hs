module Render (refresh) where

import CursesWrapper

refresh :: [[Int]] -> IO ()
refresh = do