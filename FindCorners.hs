module FindCorners
  ( findCorners
  ) where

import qualified Data.Set as Set
import Data.List (sortOn)


findCorners :: Set.Set (Int, Int) -> [(Int, Int)]
findCorners boundary =
  map (\(guess, outside) ->
        elaborateCorner boundary guess outside searchRadiusSq
      ) $
    [ ((l, t), (-1, -1))
    , ((r, t), ( 1, -1))
    , ((l, b), (-1,  1))
    , ((r, b), ( 1,  1))
    ]
  where
    boundaryList = Set.toList boundary
    (t, b) = topBottomEdges boundaryList [ymin..ymax]
    (l, r) = topBottomEdges (map flipPair boundaryList) [xmin..xmax]
    flipPair (x, y) = (y, x)
    xmin = minimum . map fst $ boundaryList
    xmax = maximum . map fst $ boundaryList
    ymin = minimum . map snd $ boundaryList
    ymax = maximum . map snd $ boundaryList
    width  = xmax - xmin
    height = ymax - ymin
    searchRadiusSq = (width*height) `div` (8^2)
    

topBottomEdges :: [(Int, Int)] -> [Int] -> (Int, Int)
topBottomEdges list ys = (top, bot)
  where
    xsAtY :: Int -> [Int]
    xsAtY y = map fst . filter ((==y) . snd) $ list
    widthAtY y  = maximum (xsAtY y) - minimum (xsAtY y)
    maxWidth  = maximum [ widthAtY y  | y <- ys ]
    wideYs = filter ((>=(maxWidth`div`2)) . widthAtY) ys
    top = head wideYs
    bot = last wideYs

elaborateCorner ::
  Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
elaborateCorner boundary guess outside radiusSq = winner
  where
    candidates = filter ((<=radiusSq) . distSq guess)
      . Set.toList $ boundary
    (_, winners) = maximaOn (dot outside) candidates
    winner = medianOn (dot $ rotate outside) winners

medianOn :: (Ord b) => (a -> b) -> [a] -> a
medianOn f list = sortOn f list !! (length list `div` 2)

maximaOn :: (Ord b) => (a -> b) -> [a] -> (b, [a])
maximaOn f = \(x:xs) -> go xs (f x, [x])
 where
   go [] (b, ms) = (b, ms)
   go (x:xs) (b, ms) = let b' = f x in
     case compare b' b of
       GT -> go xs (b', [x])
       EQ -> go xs (b, x:ms)
       LT -> go xs (b, ms)

dot :: (Int, Int) -> (Int, Int) -> Int
dot (x1, y1) (x2, y2) = x1*x2 + y1*y2

distSq :: (Int, Int) -> (Int, Int) -> Int
distSq (x1, y1) (x2, y2) = (x1-x2)^2 + (y1-y2)^2

rotate :: (Int, Int) -> (Int, Int)
rotate (x, y) = (-y, x)
