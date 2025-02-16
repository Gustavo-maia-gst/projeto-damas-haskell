{-# LANGUAGE TemplateHaskell #-}

module GameState (makeInitialState, GameState(..), Cell(..), Player(..), isSelected, available, player, isKing, cursor, selected, matrix, p1Count, p2Count, turn) where
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

makeInitialState :: GameState
makeInitialState = GameState
  { _cursor   = (7, 0)          
  , _selected = Nothing         
  , _matrix   = makeInitialMatrix   
  , _p1Count  = 12              
  , _p2Count  = 12              
  , _turn     = P1              
  }

makeInitialMatrix :: [[Cell]]
makeInitialMatrix = 
  [[
    makeCell i j | j <- [0 .. 8]
  ] | i <- [0 .. 8]]

makeCell :: Int -> Int -> Cell
makeCell i j
    | i >= 5 && (i + j) `mod` 2 == 0   = defaultCell { _player = Just P1 }
    | i <= 2 && (i + j) `mod` 2 == 0   = defaultCell { _player = Just P2 }
    | otherwise                  = defaultCell

defaultCell :: Cell
defaultCell = Cell 
  { _isSelected = False
  , _available  = False
  , _player     = Nothing
  , _isKing     = False
  }