{-# LANGUAGE TemplateHaskell #-}

module GameState (GameState(..), Turn(..), cursor, selected, matrix, p1Count, p2Count, turn) where
import Control.Lens


data Turn = P1 | P2 deriving (Show, Eq)

data GameState = GameState
  { _cursor    :: (Int, Int)      -- Posição do cursor na matriz.
  , _selected  :: (Int, Int, Bool) -- Tupla com posição e booleano.
  , _matrix    :: [[Int]]           -- Representação do tabuleiro.
  , _p1Count   :: Int             -- Número de peças do jogador P1.
  , _p2Count   :: Int             -- Número de peças do jogador P2.
  , _turn      :: Turn            -- Indica de quem é a vez.
  } deriving (Show, Eq)

makeLenses ''GameState
