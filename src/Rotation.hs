module Rotation
  ( Rotation
  , invertRotation
  , mkRotation
  , rotateDirLeft
  , rotateDir
  , rotate
  ) where

import Directions


newtype Rotation = Rotation Int

mkRotation :: Int -> Rotation
mkRotation n = Rotation (n `mod` 4)

instance Semigroup Rotation where
  Rotation a <> Rotation b = mkRotation (a+b)

instance Monoid Rotation where
  mempty = mkRotation 0

invertRotation :: Rotation -> Rotation
invertRotation (Rotation a) = mkRotation (-a)

rotateDirLeft :: Direction -> Direction
rotateDirLeft East  = North
rotateDirLeft North = West
rotateDirLeft West  = South
rotateDirLeft South = East

rotateDir :: Rotation -> Direction -> Direction
rotateDir (Rotation 0) d = d
rotateDir (Rotation n) d = rotateDir (Rotation (n-1)) (rotateDirLeft d)

rotate :: Rotation -> Directions a -> Directions a
rotate r x = directionsFromFunction
  (\d -> direction (rotateDir (invertRotation r) d) x)
