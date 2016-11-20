module State where

import WaitingToStart.State
import PlayingState.State
import Types

initialState = WaitingToStart WaitingToStart.State.initialState

update :: Float -> World -> World
update dt world =
    case world of
        WaitingToStart model -> WaitingToStart.State.update dt model
        Playing model -> PlayingState.State.update dt model
