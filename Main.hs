module Main where

import EdgesFromImage
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

replacePixels :: (Pixel px) =>
  px -> Image px -> Set.Set (Int, Int) -> Image px
replacePixels p img s =
  generateImage generator (imageWidth img) (imageHeight img)
  where
    generator x y = if (x, -y) `Set.member` s then p else pixelAt img x y
