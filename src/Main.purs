module Main where

import Prelude

import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.State
import Control.Monad.State.Class
import Data.Array (drop, length, take, (:), filter, cons)
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (traverse_, foldr, sum)
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

countPixels2 pixels = length $ map isWhite $ partition 4 pixels

isWhite :: Array Number -> Boolean
isWhite _ = true

countPixels :: Array Number -> Int
countPixels pixels = withPixels pixels single (+) 0
  where single _ = 1

countWhitePixels :: Array Number -> Int
countWhitePixels pixels = withPixels pixels isWhite (+) 0
  where isWhite xs = if sum xs > 254.0 * 4.0 then 1 else 0

lastPixel :: Array Number -> Array Number
lastPixel pixels = withPixels pixels id (\a b -> a) []

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
            else { pixel: [], result: combine (withPixel (cons pc state.pixel)) state.result }


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

    getImageData  0.0 0.0 width height

  logAny $ countWhitePixels (toArray imageData.data)

  logAny $ lastPixel (toArray imageData.data)
    
arcAboveRobot r = do
  moveTo (centerX-r) centerY
  arc { x: centerX, y: centerY, r: r, start: Math.pi, end: 0.0 }
  stroke