module PlayingState.View where

import           Data.Array
import           Data.Function (on)
import           Data.List (sortBy, groupBy)
import           Graphics.Gloss
import           Graphics.Gloss.Data.Picture
import           Lens.Micro

import           PlayingState.Types

view :: Model -> Picture
view model = pictures
    [ Translate (-130) 280 $ viewBoard (model ^. board)
    , Translate 200 200 $ color white $ viewClearedLines (model ^. clearedLines)
    ]

rows :: Board -> [[((Int, Int), Cell)]]
rows board = (groupBy (\((a, _), _) ((b, _), _) -> a == b) . sortBy (compare `on` (fst . fst))) $ zip (indices board) (elems board)

viewBoard :: Board -> Picture
viewBoard board =
    let elementsToRender = (concat . drop 2) $ rows board
    in
        pictures $ fmap viewBlock elementsToRender

viewBlock :: ((Int, Int), Cell) -> Picture
viewBlock ((i, j), cell) =
    Translate (fromIntegral $ cellSize * j) (fromIntegral $ -(cellSize * i)) $ viewCell cell

viewCell :: Cell -> Picture
viewCell cell =
    case cell of
        Empty -> color white $ rectangleWire (fromIntegral cellSize) (fromIntegral cellSize)
        Ghost -> color (greyN 0.5) $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)
        Occupied colorValue -> color colorValue $ rectangleSolid (fromIntegral cellSize) (fromIntegral cellSize)

viewClearedLines :: Int -> Picture
viewClearedLines clearedLines =
    pictures [Translate (-25) 30 $ Scale 0.15 0.15 $ Text "Lines", Scale 0.15 0.15 $ Text $ show clearedLines]
