module Main where

import FindCorners

-- (using JuicyPixels)
import Codec.Picture
import qualified Data.Set as Set


main :: IO ()
main = do
  Right img <- fmap (convertRGB8) <$> readImage "test.jpg"
  let w = imageWidth img
      h = imageHeight img
      component = fillInComponent img (w `div` 2, h `div` 2)
      red = PixelRGB8 255 0 0
      corners = findCorners $ boundary component
      pixels = Set.fromList $ corners ++ foldMap neighbours corners
      marked = replacePixels red img pixels
  print corners
  saveBmpImage "out.bmp" (ImageRGB8 marked)

boundary :: Set.Set (Int, Int) -> Set.Set (Int, Int)
boundary s = Set.filter (any (`Set.notMember` s) . neighbours) s

replacePixels :: (Pixel px) =>
  px -> Image px -> Set.Set (Int, Int) -> Image px
replacePixels p img s =
  generateImage generator (imageWidth img) (imageHeight img)
  where
    generator x y = if (x, y) `Set.member` s then p else pixelAt img x y

fillInComponent :: Image PixelRGB8 -> (Int, Int) -> Set.Set (Int, Int)
fillInComponent img start = go [start] Set.empty Set.empty
  where
    go [] inSet doneSet = inSet
    go (work:workList) inSet doneSet =
      if work `Set.member` doneSet
      then go workList inSet doneSet
      else if isIn img work
        then go (neighbours work ++ workList) (Set.insert work inSet)
               (Set.insert work doneSet)
        else go workList inSet (Set.insert work doneSet)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

isIn :: Image PixelRGB8 -> (Int, Int) -> Bool
isIn img (x, y) = isInsideImage img (x, y) && isDark (pixelAt img x y)

isInsideImage :: Image a -> (Int, Int) -> Bool
isInsideImage img (x, y) =
     x >= 0 && x < imageWidth  img
  && y >= 0 && y < imageHeight img

isDark :: PixelRGB8 -> Bool
isDark (PixelRGB8 r g b) = (r `div` 3 + g `div` 3 + b `div` 3) < 64
