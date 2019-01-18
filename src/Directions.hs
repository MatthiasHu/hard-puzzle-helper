module Directions
  ( Direction(..)
  , allDirections
  , oppositeDirection
  , move
  , Directions(..)
  , directionsFromFunction
  , direction
  ) where

import Control.Applicative


data Direction = East | North | West | South
  deriving (Show)

allDirections = [East, North, West, South]

oppositeDirection :: Direction -> Direction
oppositeDirection East  = West
oppositeDirection North = South
oppositeDirection West  = East
oppositeDirection South = North

move :: Direction -> (Int, Int) -> (Int, Int)
move East  (x, y) = (x+1, y  )
move North (x, y) = (x  , y+1)
move West  (x, y) = (x-1, y  )
move South (x, y) = (x  , y-1)

data Directions a = Directions
  { east  :: a
  , north :: a
  , west  :: a
  , south :: a
  }
  deriving (Show)

instance Functor Directions where
  fmap f (Directions e n w s) = Directions (f e) (f n) (f w) (f s)

instance Applicative Directions where
  pure a = Directions a a a a
  (Directions f0 f1 f2 f3) <*> (Directions x0 x1 x2 x3) =
    Directions (f0 x0) (f1 x1) (f2 x2) (f3 x3)

instance Foldable Directions where
  foldMap f (Directions e n w s) = f e <> f n <> f w <> f s

instance Traversable Directions where
  traverse f (Directions e n w s) =
    Directions <$> f e <*> f n <*> f w <*> f s

directionsFromFunction :: (Direction -> a) -> Directions a
directionsFromFunction f = Directions
  (f East) (f North) (f West) (f South)

direction :: Direction -> Directions a -> a
direction East  = east
direction North = north
direction West  = west
direction South = south
