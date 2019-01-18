module EdgesFromImage
  ( edgesFromImage
  , edgesAndCornersFromImage
  , edge64ToImage
  ) where

import FindCorners
import ComponentSet
import Edge
import SquareGrid
import Directions

import Codec.Picture


edgesAndCornersFromImage :: Image PixelRGB8 -> (Directions Edge64, [(Int, Int)])
edgesAndCornersFromImage img =
  ( fmap (uncurry $ edgeFromSet component) $ directionsFromFunction
      ( \dir -> case dir of
        East -> (rt, rb)
        North -> (lt, rt)
        West -> (lb, lt)
        South -> (rb, lb)
      )
  , corners
  )
  where
    w = imageWidth img
    h = imageHeight img
    component = componentSet img isDark (w `div` 2, h `div` 2)
    corners = findCorners $ boundaryMembers component
    [lb, rb, lt, rt] = corners

isDark :: PixelRGB8 -> Bool
isDark (PixelRGB8 r g b) = (r `div` 3 + g `div` 3 + b `div` 3) < 64

edgesFromImage :: Image PixelRGB8 -> Directions Edge64
edgesFromImage = fst . edgesAndCornersFromImage

edge64ToImage :: Edge64 -> Image PixelRGB8
edge64ToImage e = generateImage generator 64 64
  where
    generator x y = if edge64Pixel e (x, (63-y)) then up else down
    up = PixelRGB8 255 255 255
    down = PixelRGB8 0 0 0
