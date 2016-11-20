module Input where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           PlayingState.Input
import           Types
import           WaitingToStart.Input

input :: Event -> World -> World
input e world =
    case world of
        WaitingToStart model -> WaitingToStart $ WaitingToStart.Input.input e model
        Playing model -> Playing $ PlayingState.Input.input e model
