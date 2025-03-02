module TestMovementFinder where

import Test.HUnit
import MovementFinder
import GameState
import Control.Lens
import Utils


-- Pieces

noPiece :: Cell
noPiece = Cell
    { _isSelected = False
    , _isAvailable = False
    , _player = Nothing
    , _isKing = False
    }

player1 :: Cell
player1 = Cell {
    _isSelected = False
    , _isAvailable = False
    , _player = Just P1
    , _isKing = False
}

player2 :: Cell
player2 = Cell {
    _isSelected = False
    , _isAvailable = False
    , _player = Just P2
    , _isKing = False
}

player1K :: Cell
player1K = Cell {
    _isSelected = False
    , _isAvailable = False
    , _player = Just P1
    , _isKing = True
}

player2K :: Cell
player2K = Cell {
    _isSelected = False
    , _isAvailable = False
    , _player = Just P2
    , _isKing = True
}

availableCell :: Cell
availableCell = Cell{ 
    _isSelected = False
    , _isAvailable = True
    , _player = Nothing
    , _isKing = False
    }



-- A single P1 piece at (0, 1) Both diagonals are empty
createTestState1 :: GameState
createTestState1 = GameState
    { _matrix = [[noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P2
    , _selected = Just (0, 1)  
    }

testFindValidMoves1 :: Test
testFindValidMoves1 = TestCase $ do
    let initialState = createTestState1  
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 1 !! 0
    let cell2 = (newState ^. matrix) !! 1 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 3

    assertEqual "Cell at (1, 0) should be isAvailable" True (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 2) should be isAvailable" True (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 3) should not be isAvailable" False (cell3 ^. isAvailable)

-- P1 piece blocked in the diagonal by allies
createTestState2 :: GameState
createTestState2 = GameState
    { _matrix = [[noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [player2, noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               
               ]
    , _turn = P2
    , _selected = Just (0, 1)  
    }

testFindValidMoves2 :: Test
testFindValidMoves2 = TestCase $ do
    let initialState = createTestState2 
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 1 !! 0
    let cell2 = (newState ^. matrix) !! 1 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 3

    assertEqual "Cell at (1, 0) should be unisAvailable" False (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 2) should be unisAvailable" False (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 3) should not be unvailable" False (cell3 ^. isAvailable)

-- Blocked in the diagonal by enemy pieces
createTestState3 :: GameState
createTestState3 = GameState
    { _matrix = [[noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [player1, noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]

               ]
    , _turn = P2
    , _selected = Just (0, 1)  
    }

testFindValidMoves3 :: Test
testFindValidMoves3 = TestCase $ do
    let initialState = createTestState3
    let newState = findValidMoves initialState False  


    let cell1 = (newState ^. matrix) !! 1 !! 0
    let cell2 = (newState ^. matrix) !! 1 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 3


    assertEqual "Cell at (1, 0) should be unisAvailable" False (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 2) should be unisAvailable, it has an enemy piece" False (cell2 ^. isAvailable)

    assertEqual "Cell at (2, 3) should be isAvailable for capture" True (cell3 ^. isAvailable)

-- One diagonal is free, the other has a capture
createTestState4 :: GameState
createTestState4 = GameState
    { _matrix = [[noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]

               ]
    , _turn = P2 
    , _selected = Just (0, 1)  
    }

testFindValidMoves4 :: Test
testFindValidMoves4 = TestCase $ do
    let initialState = createTestState4  -- your initial game state with P1 at (0, 1)
    let newState = findValidMoves initialState False  -- assuming findValidMoves updates the state

    -- Ensure we're not getting out-of-bounds errors, with !! we need to be sure the indices are correct.
    let cell1 = (newState ^. matrix) !! 1 !! 0
    let cell2 = (newState ^. matrix) !! 1 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 3
    let cel4 = (newState ^. matrix) !! 3 !! 4

    -- Assert that both cells are blocked
    assertEqual "Cell at (1, 0) should be isAvailable" True (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 2) should be unisAvailable, it has enemy piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 3) should be isAvailable, it has a capture of the (1,2) piece" True (cell3 ^. isAvailable)
    assertEqual "Cell at (3, 4) should be unisAvailable, capture ended at (2,3)" False (cel4 ^. isAvailable)
    
---------- General reusable board

createTestState5 :: GameState
createTestState5 = GameState
    { _matrix = [[player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player1, noPiece, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, player2, player2, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1, player1, noPiece, player1, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, player2]
               , [noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P2
    , _selected = Just (0, 0)  
    }

-- (0,0) possible movements
testFindValidMoves5 :: Test
testFindValidMoves5 = TestCase $ do
    let initialState = createTestState5  
    let newState = findValidMoves initialState False  


    let cell1 = (newState ^. matrix) !! 2 !! 2
    let cell2 = (newState ^. matrix) !! 1 !! 1
    let cell3 = (newState ^. matrix) !! 1 !! 0
    let cell4 = (newState ^. matrix) !! 1 !! 0

    assertEqual "Cell at (2, 2) should be isAvailable capture" True (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 1) should be unisAvailable, it has enemy piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (1, 0) should be unisAvailable, pieces move in the diagonal not upfront" False (cell3 ^. isAvailable)
    assertEqual "Cell at (0, 0) should be unisAvailable, it's the own piece" False (cell4 ^. isAvailable)


createTestState6 :: GameState
createTestState6 = GameState
    { _matrix = [[player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player1, noPiece, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, player2, player2, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1, player1, noPiece, player1, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, player2]
               , [noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (1, 1)  
    }

-- (1,1) possible movements
testFindValidMoves6 :: Test
testFindValidMoves6 = TestCase $ do
    let initialState = createTestState6
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 0 !! 2
    let cell2 = (newState ^. matrix) !! 0 !! 1
    let cell3 = (newState ^. matrix) !! 0 !! 0
    let cell4 = (newState ^. matrix) !! 1 !! 0

    assertEqual "Cell at (0, 2) should be isAvailable, it has no piece" True (cell1 ^. isAvailable)
    assertEqual "Cell at (0, 1) should be unisAvailable, pieces move in the diagonal not upfront" False (cell2 ^. isAvailable)
    assertEqual "Cell at (0, 0) should be unisAvailable, it has enemy piece" False (cell3 ^. isAvailable)
    assertEqual "Cell at (1, 0) should be unisAvailable, it's the side of the P2 piece" False (cell4 ^. isAvailable)


createTestState7 :: GameState
createTestState7 = GameState
    { _matrix = [[player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player1, noPiece, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, player2, player2, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1, player1, noPiece, player1, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, player2]
               , [noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P2
    , _selected = Just (1, 4)  
    }

-- (1,4) possible movements
testFindValidMoves7 :: Test
testFindValidMoves7 = TestCase $ do
    let initialState = createTestState7
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 2 !! 5
    let cell2 = (newState ^. matrix) !! 2 !! 3
    let cell3 = (newState ^. matrix) !! 2 !! 4
    let cell4 = (newState ^. matrix) !! 1 !! 4
    let cell5 = (newState ^. matrix) !! 0 !! 5

    assertEqual "Cell at (2, 5) should be unisAvailable, it has ally piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (2, 3) should be unisAvailable, it has ally piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 4) should be unisAvailable, piece moves diagonally not upfront" False (cell3 ^. isAvailable)
    assertEqual "Cell at (1, 4) should be unisAvailable, it's the own piece" False (cell4 ^. isAvailable)
    assertEqual "Cell at (1, 4) should be unisAvailable, it moves on the other direction" False (cell5 ^. isAvailable)

createTestState8 :: GameState
createTestState8 = GameState
    { _matrix = [[player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player1, noPiece, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, player2, player2, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1, player1, noPiece, player1, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, player2]
               , [noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (3, 4)  
    }

testFindValidMoves8 :: Test
testFindValidMoves8 = TestCase $ do
    let initialState = createTestState8
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 2 !! 3
    let cell2 = (newState ^. matrix) !! 1 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 5
    let cell4 = (newState ^. matrix) !! 1 !! 6

    assertEqual "Cell at (2, 3) should be unisAvailable, it has enemy piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 2) should be isAvailable, it is a capture" True (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 5) should be unisAvailable, it has enemy piece" False (cell3 ^. isAvailable)
    assertEqual "Cell at (1, 6) should be isAvailable, it has a capture" True (cell4 ^. isAvailable)

-- (3, 6) possible movements
createTestState9 :: GameState
createTestState9 = GameState
    { _matrix = [[player2, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player1, noPiece, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, player2, player2, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1, player1, noPiece, player1, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, player2, noPiece, noPiece, noPiece, noPiece, noPiece, player2]
               , [noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (3, 6)  
    }

testFindValidMoves9 :: Test
testFindValidMoves9 = TestCase $ do
    let initialState = createTestState9
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 2 !! 5
    let cell2 = (newState ^. matrix) !! 1 !! 4
    let cell3 = (newState ^. matrix) !! 2 !! 7

    assertEqual "Cell at (2, 5) should be unisAvailable, it has enemy piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 4) should be unisAvailable, it is being double blocked" False (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 7) should be isAvailable, it is empty" True (cell3 ^. isAvailable)




-- Testing getMoveDirection function

testP2Moves :: Test
testP2Moves = TestCase $ 
  assertEqual "P2 should move up-left and up-right" 
              [(1, -1), (1, 1)] 
              (getMoveDirections P2 False)

testP1Moves :: Test
testP1Moves = TestCase $ 
  assertEqual "P1 should move down-left and down-right" 
              [(-1, -1), (-1, 1)] 
              (getMoveDirections P1 False)

testP1MovesKing :: Test
testP1MovesKing = TestCase $ 
  assertEqual "P1 King can move in any direction" 
              [(1, -1), (1, 1), (-1 , 1), (-1, -1)]
              (getMoveDirections P1 True)

testP2MovesKing :: Test
testP2MovesKing = TestCase $ 
  assertEqual "P2 King can move in any direction" 
              [(1, -1), (1, 1), (-1 , 1), (-1, -1)]
              (getMoveDirections P2 True)

-- Test isEnemy
testIsEnemy1 :: Test
testIsEnemy1 = TestCase $
    assertEqual "player2 is enemy of P1" True (isEnemy player2 P1)

testIsEnemy2 :: Test
testIsEnemy2 = TestCase $
    assertEqual "player2 is enemy of P1" False (isEnemy player2 P2)

----- King movements

-- King can move 1 cell in all diagonals
createTestState10 :: GameState
createTestState10 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1K, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (3, 3)  
    }

testFindValidMoves10 :: Test
testFindValidMoves10 = TestCase $ do
    let initialState = createTestState10
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 3 !! 3
    let cell2 = (newState ^. matrix) !! 2 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 4
    let cell4 = (newState ^. matrix) !! 4 !! 4
    let cell5 = (newState ^. matrix) !! 4 !! 2



    assertEqual "Cell at (3, 3) should be unisAvailable, it's the own king piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (2, 2) should be isAvailable, it is empty" True (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 4) should be isAvailable, it is empty" True (cell3 ^. isAvailable)
    assertEqual "Cell at (4, 4) should be isAvailable, it is empty" True (cell4 ^. isAvailable)
    assertEqual "Cell at (4, 4) should be isAvailable, it is empty" True (cell5 ^. isAvailable)

-- King can move 1 cell in all diagonals
createTestState11 :: GameState
createTestState11 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, player1, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1K, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, player1, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (3, 3)  
    }

testFindValidMoves11 :: Test
testFindValidMoves11 = TestCase $ do
    let initialState = createTestState11
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 3 !! 3
    let cell2 = (newState ^. matrix) !! 2 !! 2
    let cell3 = (newState ^. matrix) !! 2 !! 4
    let cell4 = (newState ^. matrix) !! 4 !! 4
    let cell5 = (newState ^. matrix) !! 4 !! 2



    assertEqual "Cell at (3, 3) should be unisAvailable, it's the own king piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (2, 2) should be unisAvailable, it has ally piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (2, 4) should be unisAvailable, it has ally piece" False (cell3 ^. isAvailable)
    assertEqual "Cell at (4, 4) should be unisAvailable, it has ally piece" False (cell4 ^. isAvailable)
    assertEqual "Cell at (4, 4) should be unisAvailable, it has ally piece" False (cell5 ^. isAvailable)

-- King can capture in all directions
createTestState12 :: GameState
createTestState12 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player2, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player1K, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player2, noPiece, player2, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (3, 3)  
    }

testFindValidMoves12 :: Test
testFindValidMoves12 = TestCase $ do
    let initialState = createTestState12
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 3 !! 3
    let cell2 = (newState ^. matrix) !! 1 !! 5
    let cell3 = (newState ^. matrix) !! 1 !! 1
    let cell4 = (newState ^. matrix) !! 5 !! 5
    let cell5 = (newState ^. matrix) !! 5 !! 1



    assertEqual "Cell at (3, 3) should be unisAvailable, it's the own king piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (1, 5) should be isAvailable, it has a capture" True (cell2 ^. isAvailable)
    assertEqual "Cell at (1, 1) should be isAvailable, it has a capture" True (cell3 ^. isAvailable)
    assertEqual "Cell at (5, 5) should be isAvailable, it has a capture" True (cell4 ^. isAvailable)
    assertEqual "Cell at (5, 1) should be isAvailable, it has a capture" True (cell5 ^. isAvailable)

--- Testing MultiJumps

-- Double jump in the same direction
createTestState13 :: GameState
createTestState13 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, player2, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (5, 2)  
    }

testFindValidMoves13 :: Test
testFindValidMoves13 = TestCase $ do
    let initialState = createTestState13
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 5 !! 2
    let cell2 = (newState ^. matrix) !! 4 !! 3
    let cell3 = (newState ^. matrix) !! 3 !! 4
    let cell4 = (newState ^. matrix) !! 2 !! 5
    let cell5 = (newState ^. matrix) !! 1 !! 6



    assertEqual "Cell at (2, 5) should be unisAvailable, it's the own piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (4, 3) should be unisAvailable, it has enemy piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (3, 4) should be isAvailable, it has a capture" True (cell3 ^. isAvailable)
    assertEqual "Cell at (2, 5) should be unisAvailable, it has enemy piece" False (cell4 ^. isAvailable)
    -- assertEqual "Cell at (1, 6) should be isAvailable, it has multi capture" True (cell5 ^. isAvailable)

-- Test V type capture
createTestState14 :: GameState
createTestState14 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (5, 2)  
    }

testFindValidMoves14 :: Test
testFindValidMoves14 = TestCase $ do
    let initialState = createTestState14
    let newState = findValidMoves initialState False  

    let cell1 = (newState ^. matrix) !! 5 !! 2
    let cell2 = (newState ^. matrix) !! 4 !! 3
    let cell3 = (newState ^. matrix) !! 3 !! 4
    let cell4 = (newState ^. matrix) !! 2 !! 3
    let cell5 = (newState ^. matrix) !! 1 !! 2
    let cell6 = (newState ^. matrix) !! 4 !! 1



    assertEqual "Cell at (2, 5) should be unisAvailable, it's the own piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (4, 3) should be unisAvailable, it has enemy piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (3, 4) should be isAvailable, it has a capture" True (cell3 ^. isAvailable)
    assertEqual "Cell at (2, 3) should be unisAvailable, it has enemy piece" False (cell4 ^. isAvailable)
    -- assertEqual "Cell at (1, 2) should be isAvailable, it has multi capture" True (cell5 ^. isAvailable)
    assertEqual "Cell at (4, 1) should be isAvailable, it is an empty diagonal" True (cell6 ^. isAvailable)


--- Testing MultiJump Version Only

createTestState15 :: GameState
createTestState15 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (5, 2)  
    }

testFindValidMoves15 :: Test
testFindValidMoves15 = TestCase $ do
    let initialState = createTestState15
    let newState = findValidMoves initialState True  

    let cell1 = (newState ^. matrix) !! 5 !! 2
    let cell2 = (newState ^. matrix) !! 4 !! 3
    let cell3 = (newState ^. matrix) !! 3 !! 4
    let cell4 = (newState ^. matrix) !! 2 !! 3
    let cell5 = (newState ^. matrix) !! 1 !! 2
    let cell6 = (newState ^. matrix) !! 4 !! 1



    assertEqual "Cell at (2, 5) should be unisAvailable, it's the own piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (4, 3) should be unisAvailable, it has enemy piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (3, 4) should be isAvailable, it has a capture" True (cell3 ^. isAvailable)
    assertEqual "Cell at (2, 3) should be unisAvailable, it has enemy piece" False (cell4 ^. isAvailable)
    assertEqual "Cell at (4, 1) should not be available, it is an empty diagonal" False (cell6 ^. isAvailable)

createTestState16 :: GameState
createTestState16 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, player2, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, player1, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (5, 2)  
    }

testFindValidMoves16 :: Test
testFindValidMoves16 = TestCase $ do
    let initialState = createTestState16
    let newState = findValidMoves initialState True  

    let cell1 = (newState ^. matrix) !! 5 !! 2
    let cell2 = (newState ^. matrix) !! 4 !! 3
    let cell3 = (newState ^. matrix) !! 3 !! 4
    let cell4 = (newState ^. matrix) !! 2 !! 3
    let cell5 = (newState ^. matrix) !! 1 !! 2
    let cell6 = (newState ^. matrix) !! 4 !! 1



    assertEqual "Cell at (2, 5) should be unisAvailable, it's the own piece" False (cell1 ^. isAvailable)
    assertEqual "Cell at (4, 3) should be unisAvailable, it has enemy piece" False (cell2 ^. isAvailable)
    assertEqual "Cell at (3, 4) should be isAvailable, it has a capture" False (cell3 ^. isAvailable)
    assertEqual "Cell at (2, 3) should be unisAvailable, it has enemy piece" False (cell4 ^. isAvailable)
    assertEqual "Cell at (4, 1) should not be available, it is an empty diagonal" False (cell6 ^. isAvailable)


-- Testing hasAvailableMove


createTestState17 :: GameState
createTestState17 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, availableCell, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    }

testHasAvailable1 :: Test
testHasAvailable1 = TestCase $
    assertEqual "There is at least one available cell" True (hasAvailableMove (createTestState17 ^. matrix))

createTestState18 :: GameState
createTestState18 = GameState
    { _matrix = [[noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               , [noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece, noPiece]
               ]
    , _turn = P1
    , _selected = Just (5, 2)  
    }

testHasAvailable2 :: Test
testHasAvailable2 = TestCase $
    assertEqual "There are no available cells" False (hasAvailableMove (createTestState18 ^. matrix))

