module GameState (GameState(..), Selection(..), Turn(..), Board) where
  
data Turn = P1 | P2 deriving (Show, Eq)

data Selection = NoSelection | Selected (Int, Int) deriving (Show, Eq)

type Board = [[Int]]

data GameState = GameState
  { cursor    :: (Int, Int)  -- Posição do cursor na matriz.
  , selected  :: Selection   -- Posição selecionada.
  , matrix    :: Board       -- Representação do tabuleiro.
  , p1Count   :: Int         -- Número de peças do jogador P1.
  , p2Count   :: Int         -- Número de peças do jogador P2.
  , turn      :: Turn        -- Indica de quem é a vez.
  } deriving (Show, Eq)
