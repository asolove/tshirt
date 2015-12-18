module Main where

import Prelude

import Control.Monad.Eff.Console.Unsafe (logAny)
import Data.Array (cons, length)
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable
import Data.Tuple
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

countPixels :: Array Number -> Int
countPixels pixels = foldPixels single (+) 0 pixels
  where single _ = 1

countWhitePixels :: Array Number -> Int
countWhitePixels pixels = foldPixels isWhite (+) 0 pixels
  where isWhite [r,g,b,a] = if r + g + b > 254.0 * 3.0 then 1 else 0

lastPixel :: Array Number -> Array Number
lastPixel pixels = foldPixels id (\a b -> a) [] pixels

-- | Reduce an array of pixel data to a single value
-- |  -  f: a function given an array of four ints, [r,g,b,a] that returns a value for that pixel
-- |  -  c: a fold function that combines each pixel's value into the final result
-- |  -  u: the starting value for the fold
-- |  -  xs: the array of pixel data
foldPixels :: forall a b c f. (Foldable f) => (Array a -> b) -> (b -> c -> c) -> c -> f a -> c
foldPixels = foldrN 4

foldrN :: forall a b c f. (Foldable f) =>
              Int ->
              (Array a -> b) ->
              (b -> c -> c) ->
              c ->
              f a ->
              c
foldrN n f c u xs = snd (foldr step start xs)
  where start = Tuple [] u
        step x (Tuple xs res) | length xs == n-1 = Tuple [] (c (f (cons x xs)) res)
                              | otherwise        = Tuple (cons x xs) res

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

  runGraphics context $ do
    putImageData imageData centerX 0.0
    
arcAboveRobot r = do
  moveTo (centerX-r) centerY
  arc { x: centerX, y: centerY, r: r, start: Math.pi, end: 0.0 }
  stroke