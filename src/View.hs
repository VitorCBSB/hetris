module View where

import Graphics.Gloss

import WaitingToStart.View
import PlayingState.View

import Types

view :: World -> Picture
view world =
    case world of
        WaitingToStart model -> WaitingToStart.View.view model
        Playing model -> PlayingState.View.view model
