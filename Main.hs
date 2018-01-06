module Main where

import Edge
import FindCorners
import ComponentSet
import SquareGrid

-- (using JuicyPixels)
import Codec.Picture
import qualified Data.Set as Set
import Control.Monad


main :: IO ()
main = do
  Right img <- fmap (convertRGB8) <$> readImage "test.jpg"
  let (edges, corners) = edgesAndCornersFromImage img
      cornersImg = replacePixels (PixelRGB8 255 0 0) img
        (Set.fromList $ corners ++ concatMap neighbours [head corners])
  saveBmpImage "corners.bmp" $ ImageRGB8 cornersImg
  forM_ (zip edges [0..]) $ \(edge, i) ->
    saveBmpImage ("edge-"++show i++".bmp")
      (ImageRGB8 $ edge64ToImage edge)

edgesAndCornersFromImage :: Image PixelRGB8 -> ([Edge64], [(Int, Int)])
edgesAndCornersFromImage img =
  ( map (uncurry $ edgeFromSet component)
      [ (lt, rt)
      , (lb, lt)
      , (rb, lb)
      , (rt, rb)
      ]
  , corners
  )
  where
    w = imageWidth img
    h = imageHeight img
    component = componentSet img isDark (w `div` 2, h `div` 2)
    corners = findCorners $ boundary component
    [lb, rb, lt, rt] = corners

isDark :: PixelRGB8 -> Bool
isDark (PixelRGB8 r g b) = (r `div` 3 + g `div` 3 + b `div` 3) < 64

edgesFromImage :: Image PixelRGB8 -> [Edge64]
edgesFromImage = fst . edgesAndCornersFromImage

replacePixels :: (Pixel px) =>
  px -> Image px -> Set.Set (Int, Int) -> Image px
replacePixels p img s =
  generateImage generator (imageWidth img) (imageHeight img)
  where
    generator x y = if (x, -y) `Set.member` s then p else pixelAt img x y

edge64ToImage :: Edge64 -> Image PixelRGB8
edge64ToImage e = generateImage generator 64 64
  where
    generator x y = if edge64Pixel e (x, (63-y)) then up else down
    up = PixelRGB8 255 255 255
    down = PixelRGB8 0 0 0
