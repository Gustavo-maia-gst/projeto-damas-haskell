module InitialScreen where

import CursesWrapper
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import Control.Monad (when)

printCentered :: String -> Int -> IO ()
printCentered text y = do
    (height, width) <- scrSize 
    let x = (width - length text) `div` 2 
    mvWAddStr stdScr y x text 

printTexts :: [String] -> Int -> Int -> IO ()
printTexts [] _ _ = return ()
printTexts (t:ts) indAt indCursor = do
    if indAt == indCursor 
        then printCentered ("-> " ++ t) (5 + 2 * indAt)
        else printCentered t (5 + 2 * indAt)
    printTexts ts (indAt + 1) indCursor

textHowToPlay :: String
textHowToPlay = "Para se mover utilize as teclas W, A, S, D, ou as setas\nPara selecionar use a tecla ESPAÇO ou ENTER\nAo selecionar uma peça aparecerá os possiveis movimentos para ela, caso seja de múltiplos movimentos terá que fazer movimento por movimento\nAo virar dama poderá se mover para as 4 diagonais"

howPlay :: IO()
howPlay = do
    wclear stdScr
    
    (height, width) <- scrSize 
    mvWAddStr stdScr 3 0 textHowToPlay
    
    let text = "-> Voltar"

    let x = (width - length text ) `div` 2  
    mvWAddStr stdScr (height-1) x text

    refresh

    key <- getCh  
    when (key /= KeyChar ' ' && key /= KeyChar '\n') howPlay 

options :: Int -> IO Int
options x = do
    wclear stdScr
    printCentered "Mova com W,S ou Up, Down. Para selecionar aperte ESPAÇO ou ENTER" 0
    printTexts ["Contra outro jogador", "Contra Plinio (bot)", "Como jogar"] 0 x
    refresh
    key <- getCh  

    case key of
        KeyChar 'w' -> options (max 0 (x - 1))  
        KeyUp -> options (max 0 (x - 1))  

        KeyChar 's' -> options (min 2 (x + 1))  
        KeyDown -> options (min 2 (x + 1))  

        KeyChar ' ' -> if x == 2
                       then do
                           howPlay
                           options 2
                       else return x
        KeyChar '\n' -> if x == 2
                       then do
                           howPlay
                           options 2
                       else return x
        _ -> options x
