module Main where

import Prelude

import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.State
import Control.Monad.State.Class
import Data.Array (drop, length, take, (:), filter, cons)
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (traverse_, foldr)
import Data.Maybe.Unsafe (fromJust)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free


height :: Number
height = 250.0

width :: Number
width = 500.0

centerX :: Number
centerX = 250.0

centerY :: Number
centerY = 200.0

type Color = { r :: Number, g :: Number, b :: Number, a :: Number }

partition :: forall a. Int -> Array a -> Array (Array a)
partition n as = if length as < n
                  then []
                  else take n as : partition n (drop n as)

colors :: Graphics.Canvas.ImageData -> Array Color
colors imageData = map toColor quartets
  where toColor [r,g,b,a] = { r: r, g: g, b: b, a: a }
        quartets = partition 4 (toArray imageData.data)

isWhite :: Color -> Boolean
isWhite _ = true

countPixels :: Array Number -> Int
countPixels pixels = withPixels pixels single (+) 0
  where single _ = 1

withPixels :: forall a b. Array Number -> 
              (Array Number -> a) ->
              (a -> b -> b) ->
              b ->
              b
withPixels pixels withPixel combine start = (foldr eachPixelComponent startState pixels).result
  where startState = { pixel: [], result: start }
        eachPixelComponent pc state = 
          if length state.pixel < 3
            then { pixel: cons pc state.pixel, result: state.result }
            else { pixel: [], result: combine (withPixel state.pixel) state.result }


main = do
  canvas <- getCanvasElementById "canvas"
  context <- getContext2D $ fromJust canvas

  imageData <- runGraphics context $ do
    -- Canvas API calls will go here
    setFillStyle "#999999"
    rect { x: 0.0, y: 0.0, w: width, h: height }
    fill

    setStrokeStyle "#FFFFFF"
    traverse_ arcAboveRobot [20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 100.0]

    getImageData 250.0 200.0 100.0 50.0

  -- logAny (length (filter isWhite (colors imageData)))
  logAny $ countPixels (toArray imageData.data)
    
arcAboveRobot r = do
  moveTo (centerX-r) centerY
  arc { x: centerX, y: centerY, r: r, start: Math.pi, end: 0.0 }
  stroke