module Main where

import EdgesFromImage
import SquareGrid

-- (using JuicyPixels)
import Codec.Picture
import System.FilePath
import System.Directory
import qualified Data.Set as Set
import Control.Monad


main :: IO ()
main = allImagesToEdges

allImagesToEdges :: IO ()
allImagesToEdges = do
  let
    imagesPath = "input/images"
    edgesPath = "input/pieces"
    extension = ".jpg"
  imagePaths <- filter ((==extension) . takeExtension) <$>
    listDirectory imagesPath
  putStrLn $ unwords
    [ "found", show (length imagePaths)
    , extension, "files in", imagesPath ]
  putStrLn $ unwords ["saving edges to", edgesPath, "..."]
  forM_ imagePaths $ \path -> do
    putStrLn path
    imageToEdges (imagesPath </> path) edgesPath

imageToEdges :: FilePath -> FilePath -> IO ()
imageToEdges imagePath edgesPath = do
  readImage imagePath >>= \imgEither -> case imgEither of
    Left err -> putStrLn $ unwords ["can't load image:", err]
    Right img -> do
      let
        es = edgesFromImage (convertRGB8 img)
        compass = ["north", "west", "south", "east"]
        name dir = takeBaseName imagePath ++ "-" ++ dir
      forM_ (zip es compass) $ \(e, dir) ->
        saveBmpImage (edgesPath </> name dir <.> "bmp")
          (ImageRGB8 $ edge64ToImage e)

debugSingleImage :: FilePath -> IO ()
debugSingleImage path = do
  Right img <- fmap (convertRGB8) <$> readImage path
  let (edges, corners) = edgesAndCornersFromImage img
      cornersImg = replacePixels (PixelRGB8 255 0 0) img
        (Set.fromList $ corners ++ concatMap neighbours [head corners])
  saveBmpImage "debug-out/corners.bmp" $ ImageRGB8 cornersImg
  forM_ (zip edges [0..]) $ \(edge, i) ->
    saveBmpImage ("debug-out/edge-"++show i++".bmp")
      (ImageRGB8 $ edge64ToImage edge)

replacePixels :: (Pixel px) =>
  px -> Image px -> Set.Set (Int, Int) -> Image px
replacePixels p img s =
  generateImage generator (imageWidth img) (imageHeight img)
  where
    generator x y = if (x, -y) `Set.member` s then p else pixelAt img x y
