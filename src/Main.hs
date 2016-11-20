module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import Types
import Input
import State
import View

main :: IO ()
main =
    play
        --(FullScreen (800, 600))
        (InWindow "Hetris" (800, 600) (10, 10))
        black
        60
        initialState
        view
        input
        update
