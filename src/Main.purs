module Main where

import Prelude

import Control.Monad.Eff.Console.Unsafe (logAny)
import Data.Array (cons, length, (..))
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable
import Data.Int (toNumber)
import Data.Tuple
import Data.Maybe.Unsafe (fromJust)
import Graphics.Canvas

height :: Number
height = 250.0

width :: Number
width = 510.0

centerX :: Number
centerX = 255.0

centerY :: Number
centerY = 230.0


countWhitePixels :: Array Number -> Int
countWhitePixels pixels = foldPixels isWhite (+) 0 pixels
  where isWhite [r,g,b,a] = if r + g + b > 254.0 * 3.0 then 1 else 0



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
  maybeCanvas <- getCanvasElementById "canvas"
  let canvas = fromJust maybeCanvas
  context <- getContext2D canvas

  setCanvasHeight height canvas
  setCanvasWidth width canvas

  withImage "tshirt.jpg" $ \img -> do
    drawImage context img 0.0 0.0

    setStrokeStyle "#FFFFFF" context
    traverse_ (arcAboveRobot context) (map ((*15) >>> toNumber) (1..16))

    imageData <- getImageData context 200.0 100.0 30.0 30.0
    logAny $ countWhitePixels (toArray imageData.data)

    c2 <- getCanvasElementById "partial"
    drawImageData imageData (fromJust c2)
    return unit

    

drawImageData imageData canvas = do
  context <- getContext2D canvas
  setCanvasWidth (toNumber imageData.width) canvas
  setCanvasHeight (toNumber imageData.height) canvas
  putImageData context imageData 0.0 0.0


arcAboveRobot context r = do
  moveTo context (centerX-r) centerY
  bezierCurveTo { cp1x: centerX-r, cp1y: centerY-(r*1.2), cp2x: centerX+r, cp2y: centerY-(r*1.2), x: centerX+r, y: centerY } context
  stroke context

