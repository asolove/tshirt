module Main where

import Prelude

import Control.Monad.Free
import Data.Foldable (traverse_)
import Data.Maybe.Unsafe (fromJust)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free
import Math

height :: Number
height = 250.0

width :: Number
width = 500.0

centerX :: Number
centerX = 250.0

centerY :: Number
centerY = 200.0

main = do
  canvas <- getCanvasElementById "canvas"
  context <- getContext2D $ fromJust canvas

  runGraphics context $ do
    -- Canvas API calls will go here
    setFillStyle "#999999"
    rect { x: 0.0, y: 0.0, w: width, h: height }
    fill

    setStrokeStyle "#FFFFFF"
    traverse_ arcAboveRobot [20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 100.0]

arcAboveRobot r = do
  moveTo (centerX-r) centerY
  arc { x: centerX, y: centerY, r: r, start: Math.pi, end: 0.0 }
  stroke