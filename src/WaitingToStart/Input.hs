module WaitingToStart.Input where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           WaitingToStart.Types

input :: Event -> Model -> Model
input (EventKey (SpecialKey KeyEnter) Down _ _) _ = True
input _ _ = False
