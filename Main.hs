-- (using JuicyPixels)
import Codec.Picture
import qualified Data.Set as Set


main :: IO ()
main = do
  Right img <- fmap (convertRGB8) <$> readImage "test.jpg"
  saveBmpImage "out.bmp" (ImageRGB8 $ markCenterComponent img)

markCenterComponent :: Image PixelRGB8 -> Image PixelRGB8
markCenterComponent img = generateImage generator w h
  where
    w = imageWidth img
    h = imageHeight img
    generator x y = let px = pixelAt img x y in
      if (x, y) `Set.member` component then redden px else px
    component = fillInComponent img (w `div` 2, h `div` 2)
    redden (PixelRGB8 r g b) = PixelRGB8 255 g b

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
