module PlayingState.State where

import Data.Array
import Data.Bifunctor
import Data.Function (on)
import Data.List
import Graphics.Gloss.Data.Color
import Lens.Micro
import System.Random

import PlayingState.Types
import Types

initialState :: Model
initialState =
  let initialSeed = mkStdGen 42
      (firstTetromino, nextSeed) = randomTetromino initialSeed
      (nextTetromino, nextSeed') = randomTetromino nextSeed
  in Model
     { _inputActions = Input (0, 0) False False
     , _board = emptyBoard
     , _score = 0
     , _clearedLines = 0
     , _playState = PlacingTetromino firstTetromino
     , _placedBlocks = []
     , _rngSeed = nextSeed'
     , _nextTetromino = nextTetromino
     }

makeTetromino :: TetrominoShape -> Tetromino
makeTetromino shape = Tetromino (initialShape shape) (1, 4) shape 0.0 0.0

randomTetromino
  :: RandomGen g
  => g -> (Tetromino, g)
randomTetromino g =
  case randomR (0, 6) g of
    (r, g') -> (makeTetromino (toEnum r), g')

tetrominoColor :: Tetromino -> Color
tetrominoColor tetromino =
  case basicShape tetromino of
    I -> cyan
    O -> yellow
    T -> makeColorI 192 5 248 255 -- purple
    S -> green
    Z -> red
    J -> blue
    L -> orange

shapePositions :: Tetromino -> [(Int, Int)]
shapePositions tetromino =
  let (tetrI, tetrJ) = position tetromino
  in map (bimap (tetrI +) (tetrJ +)) (shape tetromino)

emptyBoard :: Board
emptyBoard =
  array
    ((0, 0), (boardHeight - 1, boardWidth - 1))
    [ ((i, j), Empty)
    | i <- [0 .. boardHeight - 1]
    , j <- [0 .. boardWidth - 1] ]

-- Tetromino shapes
initialShape :: TetrominoShape -> [(Int, Int)]
initialShape tetromino =
  case tetromino of
    I -> [(0, -1), (0, 0), (0, 1), (0, 2)]
    O -> [(0, 0), (-1, 0), (-1, 1), (0, 1)]
    T -> [(-1, 0), (0, -1), (0, 0), (0, 1)]
    S -> [(0, -1), (0, 0), (-1, 0), (-1, 1)]
    Z -> [(-1, -1), (-1, 0), (0, 0), (0, 1)]
    J -> [(-1, -1), (0, -1), (0, 0), (0, 1)]
    L -> [(0, -1), (0, 0), (0, 1), (-1, 1)]

insertTetromino :: Tetromino -> Board -> Board
insertTetromino tetromino board =
  let (tetrI, tetrJ) = position tetromino
  in board //
     [ ((i', j'), Occupied (tetrominoColor tetromino))
     | (i', j') <- map (bimap (tetrI +) (tetrJ +)) (shape tetromino) ]

rotateTetromino :: Bool -> Tetromino -> Tetromino
rotateTetromino right tetromino =
  case basicShape tetromino of
    O -> tetromino
    _ ->
      if right
        then rotateTetrRight tetromino
        else rotateTetrLeft tetromino

rotateTetrLeft :: Tetromino -> Tetromino
rotateTetrLeft tetromino =
  tetromino
  { shape = map (\(i, j) -> (j, -i)) (shape tetromino)
  }

rotateTetrRight :: Tetromino -> Tetromino
rotateTetrRight tetromino =
  tetromino
  { shape = map (\(i, j) -> (-j, i)) (shape tetromino)
  }

updateTetromino :: Float
                -> Input
                -> Board
                -> Tetromino
                -> Either [((Int, Int), Color)] Tetromino
updateTetromino dt input board tetromino =
  let newTetromino =
        applyChange
          (applyRotation (input ^. rotateLeft, input ^. rotateRight))
          board .
        applyChange (applyMovement dt (input ^. directionKeys)) board $
        tetromino
      postFallTetromino = applyFall dt (input ^. directionKeys) newTetromino
  in if validateTetromino postFallTetromino board
       then Right postFallTetromino
       else Left $
            zip
              (shapePositions newTetromino)
              (repeat $ tetrominoColor newTetromino)

applyFall :: Float -> (Int, Int) -> Tetromino -> Tetromino
applyFall dt (dirI, _) tetromino =
  let (tetrI, tetrJ) = position tetromino
      actualFallCooldownTime
        | dirI == 1 = baseFallCooldownTime / 10 -- Soft drop
        | dirI == (-1) = 0.001 -- Hard drop
        | otherwise = baseFallCooldownTime
  in if fallCooldownTime tetromino > actualFallCooldownTime
       then tetromino
            { position = (tetrI + 1, tetrJ)
            , fallCooldownTime = 0.0
            }
       else tetromino
            { fallCooldownTime = fallCooldownTime tetromino + dt
            }

applyMovement :: Float -> (Int, Int) -> Tetromino -> Tetromino
applyMovement dt (_, dirJ) tetromino =
  if moveCooldownTime tetromino <= 0 && dirJ /= 0
    then tetromino
         { position = (tetrI, tetrJ + dirJ)
         , moveCooldownTime = baseMoveCooldownTime
         }
    else tetromino
         { moveCooldownTime = newMoveCooldownTime
         }
  where
    (tetrI, tetrJ) = position tetromino
    newMoveCooldownTime = max 0 (moveCooldownTime tetromino - dt)

applyRotation :: (Bool, Bool) -> Tetromino -> Tetromino
applyRotation (rotLeft, rotRight) tetromino =
  case (rotLeft, rotRight) of
    (True, False) -> rotateTetromino False tetromino
    (False, True) -> rotateTetromino True tetromino
    _ -> tetromino

applyChange :: (Tetromino -> Tetromino) -> Board -> Tetromino -> Tetromino
applyChange f board tetromino =
  let newTetromino = f tetromino
  in if validateTetromino newTetromino board
       then newTetromino
       else tetromino

validateTetromino :: Tetromino -> Board -> Bool
validateTetromino tetromino board =
  let validatePos (i, j) =
        i >= 0 &&
        j >= 0 &&
        i < boardHeight && j < boardWidth && not (isOccupied board (i, j))
  in all validatePos (shapePositions tetromino)

isOccupied :: Board -> (Int, Int) -> Bool
isOccupied board pos =
  case board ! pos of
    Occupied _ -> True
    _ -> False

insertBlocks :: [((Int, Int), Color)] -> Board -> Board
insertBlocks blocksToPlace board =
  board //
  [ ((i, j), Occupied color)
  | ((i, j), color) <- blocksToPlace ]

insertGhostBlocks :: [(Int, Int)] -> Board -> Board
insertGhostBlocks ghostToPlace board =
  board //
  [ ((i, j), Ghost)
  | (i, j) <- ghostToPlace ]

update :: Float -> Model -> World
update dt model =
  let newModel =
        case _playState model of
          PlacingTetromino tetromino -> updatePlacingTetromino dt tetromino model
          GenerateNewTetromino ->
            let (newTetromino, nextSeed) = randomTetromino (model ^. rngSeed)
            in model
               { _playState = PlacingTetromino (model ^. nextTetromino)
               , _rngSeed = nextSeed
               , _nextTetromino = newTetromino
               }
          ClearingLines clearInterval rowsToClear ->
            updateClearingLines dt clearInterval rowsToClear model
  in Playing $
     (inputActions . rotateLeft) .~ False $
     (inputActions . rotateRight) .~ False $ newModel

rowsToClearAndKeep :: [((Int, Int), Color)]
                   -> ([[((Int, Int), Color)]], [((Int, Int), Color)])
rowsToClearAndKeep placedBlocks =
  let rows =
        (groupBy (\((a, _), _) ((b, _), _) -> a == b) .
         sortBy (compare `on` (fst . fst)))
          placedBlocks
  in second concat $ partition (\r -> length r == boardWidth) rows

insertGhost :: Tetromino -> Board -> Board
insertGhost tetromino board =
  let ghostSequence =
        iterate
          (\t ->
              t
              { position = first (1 +) (position t)
              })
          tetromino
      ghostTetromino =
        (fst . head) $
        dropWhile (\(g, g') -> validateTetromino g' board) $
        zip ghostSequence (tail ghostSequence)
  in insertGhostBlocks (shapePositions ghostTetromino) board

updatePlacingTetromino :: Float -> Tetromino -> Model -> Model
updatePlacingTetromino dt tetromino model =
  let boardWithBlocks = insertBlocks (model ^. placedBlocks) emptyBoard
      updateResult =
        updateTetromino dt (model ^. inputActions) boardWithBlocks tetromino
  in case updateResult of
       Left placedTetromino ->
         let placedBlocksTetromino = _placedBlocks model ++ placedTetromino
             (rowsToClear, newPlacedBlocks) = rowsToClearAndKeep placedBlocksTetromino
             newPlayState =
               if null rowsToClear
                 then GenerateNewTetromino
                 else ClearingLines 0.0 rowsToClear
             newClearedLines = length rowsToClear
             newBoard = insertBlocks placedTetromino boardWithBlocks
         in model
            { _board = newBoard
            , _playState = newPlayState
            , _placedBlocks = newPlacedBlocks
            , _clearedLines = (model ^. clearedLines) + newClearedLines
            }
       Right newTetromino ->
         let boardWithGhost = insertGhost newTetromino boardWithBlocks
             boardWithTetromino = insertTetromino newTetromino boardWithGhost
         in model
            { _board = boardWithTetromino
            , _playState = PlacingTetromino newTetromino
            }

updateClearingLines :: Float -> Float -> [[((Int, Int), Color)]] -> Model -> Model
updateClearingLines dt clearInterval rowsToClear model =
  if clearInterval > lineClearDropTime
    then model
         { _playState = GenerateNewTetromino
         , _placedBlocks = blocksBelow ++ droppedBlocks
         , _board = insertBlocks (blocksBelow ++ droppedBlocks) emptyBoard
         }
    else model
         { _playState = ClearingLines (clearInterval + dt) rowsToClear
         , _board = insertBlocks (model ^. placedBlocks) emptyBoard
         }
  where
    linesToDrop = length rowsToClear
    -- I'm using the fact that this list is already sorted.
    lowestAffectedRow = (fst . fst . head . head) rowsToClear
    (blocksToDrop, blocksBelow) =
      partition (\((i, _), _) -> i < lowestAffectedRow) (model ^. placedBlocks)
    droppedBlocks = map (\((i, j), c) -> ((i + linesToDrop, j), c)) blocksToDrop
