module Edge
  ( Edge64()
  , mkEdge64
  , edgeFromSet
  , Cost
  , rawMatchingCost
  , rawCostToCost
  , matchingCost
  , edge64Pixel
  ) where

import FillGraph
import SquareGrid

import Data.Word
import Data.Bits hiding (rotate)
import qualified Data.Set as Set
import Data.Binary
import Data.Foldable
import Control.Monad


-- Coordinates in insideDown:
-- Low x (head of the list) is left,
-- low y (bits of low significance) is bottom (inside the puzzle piece).
data Edge64 = Edge64
  { insideDown :: [Word64]
  , insideUp   :: [Word64]
  }

validEdge64 :: Edge64 -> Bool
validEdge64 (Edge64 iU iD) =
     length iD == 64
  && iU == rotate iD

rotate :: [Word64] -> [Word64]
rotate = reverse . map reverseBits

reverseBits :: Word64 -> Word64
reverseBits w = fromBits [63-i | i <- [0..63], testBit w i]

fromBits :: [Int] -> Word64
fromBits = foldl' setBit zeroBits

mkEdge64 :: [Word64] -> Edge64
mkEdge64 iD =
  if length iD /= 64
  then error "mkEdge64 list length not 64"
  else Edge64 iD (rotate iD)

-- Probes the input function in the square [0 : 1] x [-0.5 : 0.5].
-- The inside of the puzzle piece should be "down" as in "negative y".
-- The positions (0, 0.5) and (1, 0.5) should be the corners.
generateEdge64 :: (Float -> Float -> Bool) -> Edge64
generateEdge64 inside = Edge64 iD iU
  where
    outsideSet = Set.fromList
      [ (x, y)
      | x <- [0..63], y <- [0..63]
      , not $ inside (transformX x) (transformY y) ]
      Set.\\ Set.fromList [(0, 31), (63, 31)]
    filledOutsideSet = fillGraph
      (filter (`Set.member` outsideSet) . neighbours) (32, 63)
    iD = [ fromBits [ y | y <- [0..63]
                        , (x, y) `Set.notMember` filledOutsideSet ]
         | x <- [0..63] ]
    iU = rotate iD
    transformX x = fromIntegral x * delta
    transformY y = fromIntegral y * delta - 0.5
    delta = 1 / fromIntegral 63

edgeFromSet :: Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Edge64
edgeFromSet insideSet (x1, y1) (x2, y2) = generateEdge64 f
  where
    dx = fromIntegral x2 - fromIntegral x1
    dy = fromIntegral y2 - fromIntegral y1
    toPixels x y = ( fromIntegral x1 + dx*x - dy*y
                   , fromIntegral y1 + dx*y + dy*x )
    f x y = let (xp, yp) = toPixels x y
            in (round xp, round yp) `Set.member` insideSet

type Cost = Float

rawMatchingCost :: Edge64 -> Edge64 -> Word16
rawMatchingCost e1 e2 =
  fromIntegral . sum $
    [ popCount . complement $ xor r1 r2
    | (r1, r2) <- zip (insideDown e1) (insideUp e2) ]

rawCostToCost :: Word16 -> Cost
rawCostToCost x = (0.1 * fromIntegral x)^2

matchingCost :: Edge64 -> Edge64 -> Cost
matchingCost e1 e2 = rawCostToCost (rawMatchingCost e1 e2)

-- (Retains mathematical coordinates: low y is bottom.)
edge64Pixel :: Edge64 -> (Int, Int) -> Bool
edge64Pixel (Edge64 iD _) (x, y) = testBit (iD !! x) y


-- Use encode and decode from Data.Binary.
instance Binary Edge64 where
  put (Edge64 iD _) = mapM_ put iD
  get = mkEdge64 <$> replicateM 64 get
