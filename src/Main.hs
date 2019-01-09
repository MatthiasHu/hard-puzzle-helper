module Main where

import EdgesFromImage
import SquareGrid

-- (using JuicyPixels)
import Codec.Picture
import System.FilePath
import System.Directory
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import Data.List
import Control.Monad


main :: IO ()
main = allImagesToEdges

allImagesToEdges :: IO ()
allImagesToEdges = do
  let
    imagesPath = "input/images"
    edgesPath = "input/pieces"
    imagesExtension = ".jpg"
  imageNames <- getImageNames imagesExtension imagesPath
  putStrLn $ unwords
    [ "found", show (length imageNames)
    , imagesExtension, "files in", imagesPath ]
  generateMissingEdgeFiles imagesPath edgesPath imageNames

getImageNames :: String -> FilePath -> IO [FilePath]
getImageNames extension imagesPath =
  (sort . filter ((==extension) . takeExtension)) <$>
  listDirectory imagesPath

generateMissingEdgeFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
generateMissingEdgeFiles imagesPath edgesPath allImageNames = do
  imageNames <- filterM
    (fmap not . edgeFilesPresent edgesPath . takeBaseName)
    allImageNames
  putStrLn $ unwords
    [ show (length imageNames)
    , "images lack edges files in", edgesPath ]
  putStrLn $ unwords ["saving edges to", edgesPath, "..."]
  forM_ imageNames $ \path -> do
    putStrLn path
    imageToEdges (imagesPath </> path) edgesPath

edgeFilesPresent :: FilePath -> String -> IO Bool
edgeFilesPresent edgesPath baseName = all (==True) <$>
  mapM doesFileExist (edgeFilePaths edgesPath baseName)

edgeFilePaths :: FilePath -> String -> [String]
edgeFilePaths edgesPath baseName =
  [ edgesPath </> (baseName ++ "-" ++ dir) <.> ".edge64"
  | dir <- compass ]
  where
    compass = ["north", "west", "south", "east"]

imageToEdges :: FilePath -> FilePath -> IO ()
imageToEdges imagePath edgesPath = do
  readImage imagePath >>= \imgEither -> case imgEither of
    Left err -> putStrLn $ unwords ["can't load image:", err]
    Right img -> do
      let
        es = edgesFromImage (convertRGB8 img)
        baseName = takeBaseName imagePath
      forM_ (zip es (edgeFilePaths edgesPath baseName)) $ \(e, file) ->
        encodeFile file e

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
