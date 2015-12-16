module Main where

import Prelude
import Control.Monad.Eff

import Data.Maybe.Unsafe (fromJust)

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free

main = do
  canvas <- getCanvasElementById "canvas"
  context <- getContext2D $ fromJust canvas

  runGraphics context $ do
    -- Canvas API calls will go here
    setFillStyle "#00FFFF"
    rect { x: 0.0, y: 0.0, w: 400.0, h: 600.0 }
    fill