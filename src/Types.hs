module Types where

import PlayingState.Types
import WaitingToStart.Types

data World =
    WaitingToStart WaitingToStart.Types.Model
    | Playing PlayingState.Types.Model
