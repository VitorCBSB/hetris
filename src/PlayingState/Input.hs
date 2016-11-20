module PlayingState.Input where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import Data.Map
import Lens.Micro

import PlayingState.Types

-- KEY DOWN

input :: Event -> Model -> Model

-- Movement
input (EventKey (SpecialKey KeyLeft) Down _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di, dj - 1))
input (EventKey (SpecialKey KeyUp) Down _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di - 1, dj))
input (EventKey (SpecialKey KeyRight) Down _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di, dj + 1))
input (EventKey (SpecialKey KeyDown) Down _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di + 1, dj))

-- Rotation
input (EventKey (Char 'z') Down _ _) =
    (inputActions . rotateLeft) .~ True
input (EventKey (Char 'Z') Down _ _) =
    (inputActions . rotateLeft) .~ True

input (EventKey (Char 'x') Down _ _) =
    (inputActions . rotateRight) .~ True
input (EventKey (Char 'X') Down _ _) =
    (inputActions . rotateRight) .~ True

-- KEY UP

input (EventKey (SpecialKey KeyLeft) Up _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di, dj + 1))
input (EventKey (SpecialKey KeyUp) Up _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di + 1, dj))
input (EventKey (SpecialKey KeyRight) Up _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di, dj - 1))
input (EventKey (SpecialKey KeyDown) Up _ _) =
    (inputActions . directionKeys) %~ (\(di, dj) -> (di - 1, dj))

input _ = id
