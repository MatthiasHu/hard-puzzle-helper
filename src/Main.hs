module Main where

import Matching
import EdgesFromImage
import Piece
import Directions
import SquareGrid

-- (using JuicyPixels)
import Codec.Picture
import System.FilePath
import System.Directory
import Data.Binary
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.List
import Data.Foldable
import Data.Char
import Control.Monad
import Control.Applicative


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
  matchingData <- loadPieces edgesPath imageNames
  putStrLn "best edge matchings:"
  mapM_ print (take 100 $ bestEdgeMatchings matchingData)

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

loadPieces :: FilePath -> [String] -> IO (V.Vector Piece)
loadPieces edgesPath imageNames = do
  putStrLn $ unwords ["loading pieces from", edgesPath, "..."]
  V.fromList <$> mapM (loadPiece edgesPath . takeBaseName) imageNames

loadPiece :: FilePath -> String -> IO Piece
loadPiece edgesPath imageBaseName = sequence $
  (\path -> decodeFile path) <$>
  edgeFilePaths edgesPath imageBaseName

edgeFilePaths :: FilePath -> String -> Directions FilePath
edgeFilePaths edgesPath baseName = directionsFromFunction (\dir ->
  edgesPath </> (baseName ++ "-" ++ (dirString dir)) <.> ".edge64" )
  where
    dirString = map toLower . show

imageToEdges :: FilePath -> FilePath -> IO ()
imageToEdges imagePath edgesPath = do
  readImage imagePath >>= \imgEither -> case imgEither of
    Left err -> putStrLn $ unwords ["can't load image:", err]
    Right img -> do
      let
        edges = edgesFromImage (convertRGB8 img)
        filePaths = edgeFilePaths edgesPath (takeBaseName imagePath)
      sequence_ $ liftA2 encodeFile filePaths edges

debugSingleImage :: FilePath -> IO ()
debugSingleImage path = do
  Right img <- fmap (convertRGB8) <$> readImage path
  let (edges, corners) = edgesAndCornersFromImage img
      cornersImg = replacePixels (PixelRGB8 255 0 0) img
        (Set.fromList $ corners ++ concatMap neighbours [head corners])
  saveBmpImage "debug-out/corners.bmp" $ ImageRGB8 cornersImg
  forM_ (zip (toList edges) [0..]) $ \(edge, i) ->
    saveBmpImage ("debug-out/edge-"++show i++".bmp")
      (ImageRGB8 $ edge64ToImage edge)

replacePixels :: (Pixel px) =>
  px -> Image px -> Set.Set (Int, Int) -> Image px
replacePixels p img s =
  generateImage generator (imageWidth img) (imageHeight img)
  where
    generator x y = if (x, -y) `Set.member` s then p else pixelAt img x y
