module WaitingToStart.View where

import Graphics.Gloss

import WaitingToStart.Types

view :: Model -> Picture
view _ = color white $ Translate (-200) 0 $ Scale 0.3 0.3 $ Text "Press Enter to start!"
