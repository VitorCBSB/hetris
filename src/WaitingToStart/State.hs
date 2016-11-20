module WaitingToStart.State where

import PlayingState.State

import WaitingToStart.Types
import Types

initialState :: Model
initialState = False

update :: Float -> Model -> World
update dt model =
    case model of
        False -> WaitingToStart False
        True -> Playing PlayingState.State.initialState
