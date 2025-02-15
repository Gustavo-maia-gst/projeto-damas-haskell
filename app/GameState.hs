{-# LANGUAGE TemplateHaskell #-}

module GameState (GameState(..), Cell(..), Player(..), isSelected, available, player, isKing, cursor, selected, matrix, p1Count, p2Count, turn) where
import Control.Lens


data Player = P1 | P2 deriving (Show, Eq)

data Cell = Cell
  { _isSelected     :: Bool            -- Indica se o cursor está na célula
  , _available      :: Bool            -- Indica se é possível mover para esta célula
  , _player         :: Maybe Player    -- Indica o jogador da peça
  , _isKing         :: Bool            -- Indica de quem é a vez.
  } deriving (Show, Eq)

makeLenses ''Cell

data GameState = GameState
  { _cursor    :: (Int, Int)        -- Posição do cursor na matriz.
  , _selected  :: Maybe (Int, Int)  -- Tupla com posição e booleano.
  , _matrix    :: [[Cell]]           -- Representação do tabuleiro.
  , _p1Count   :: Int               -- Número de peças do jogador P1.
  , _p2Count   :: Int               -- Número de peças do jogador P2.
  , _turn      :: Player            -- Indica de quem é a vez.
  } deriving (Show, Eq)

makeLenses ''GameState
