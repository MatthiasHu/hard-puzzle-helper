module ComponentSet
  ( componentSet
  ) where

import FillGraph
import SquareGrid

import Codec.Picture
import qualified Data.Set as Set


-- In the component set mathematical coordinates are used: positive y is
-- upwards. The y coordinates are therefore negative, but who cares?
componentSet :: Image PixelRGB8 -> (PixelRGB8 -> Bool) -> (Int, Int) ->
  Set.Set (Int, Int)
componentSet img isDark (startx, starty) =
  fillGraph (filter (isIn img) . neighbours) (startx, -starty)
  where
    isIn :: Image PixelRGB8 -> (Int, Int) -> Bool
    isIn img (x, y) = isInsideImage img (x, -y) && isDark (pixelAt img x (-y))

isInsideImage :: Image a -> (Int, Int) -> Bool
isInsideImage img (x, y) =
     x >= 0 && x < imageWidth  img
  && y >= 0 && y < imageHeight img
