{-# LANGUAGE TemplateHaskell #-}

module PlayingState.Types where

import Graphics.Gloss.Data.Color
import Data.Array
import Lens.Micro.TH
import System.Random

boardWidth = 10 :: Int
boardHeight = 22 :: Int

cellSize = 25 :: Int -- in pixels

baseMoveCooldownTime = 0.2 :: Float -- in seconds
baseFallCooldownTime = 0.6 :: Float -- in seconds

lineClearDropTime = 0.5 :: Float -- in seconds

data TetrominoShape =
    I
    | O
    | T
    | S
    | Z
    | J
    | L
    deriving (Enum)

data Tetromino = Tetromino
    { shape :: [(Int, Int)]
    , position :: (Int, Int)
    , basicShape :: TetrominoShape
    , moveCooldownTime :: Float
    , fallCooldownTime :: Float
    }

data Cell =
    Empty
    | Ghost
    | Occupied Color

type Board = Array (Int, Int) Cell

data Input = Input
    { _directionKeys :: (Int, Int)
    , _rotateRight :: Bool -- On press input
    , _rotateLeft :: Bool -- On press input
    }

data PlayState =
    PlacingTetromino Tetromino
    | ClearingLines Float [[((Int, Int), Color)]]
    | GenerateNewTetromino

data Model = Model
    { _inputActions :: Input
    , _board :: Board
    , _score :: Int
    , _clearedLines :: Int
    , _playState :: PlayState
    , _placedBlocks :: [((Int, Int), Color)]
    , _rngSeed :: StdGen
    , _nextTetromino :: Tetromino
    }

makeLenses ''Model
makeLenses ''Input
