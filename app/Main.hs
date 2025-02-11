{-# LANGUAGE OverloadedStrings #-}

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Monad (when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  initCurses
  cBreak True          -- Habilita o modo cbreak (desativa buffering de linha)
  echo False           -- Desabilita o echo (equivalente a noecho())
  keypad stdScr True   -- Ativa leitura de teclas especiais na tela padrão
  cursSet CursorInvisible  -- Oculta o cursor
  loop (10, 10)        -- Posição inicial (linha 10, coluna 10)
  endWin

loop :: (Int, Int) -> IO ()
loop (y, x) = do
  wclear stdScr  -- Limpa a tela padrão
  mvAddCh y x (fromIntegral (fromEnum '@'))
  refresh
  key <- getCh  -- Lê a tecla pressionada (do tipo Key)
  let (ny, nx) = case key of
                   KeyChar 'w' -> (max 0 (y - 1), x)
                   KeyChar 's' -> (y + 1, x)
                   KeyChar 'a' -> (y, max 0 (x - 1))
                   KeyChar 'd' -> (y, x + 1)
                   _           -> (y, x)
  if key == KeyChar 'q'
    then endWin >> exitSuccess
    else loop (ny, nx)
