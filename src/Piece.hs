module Piece
  ( Piece(..)
  ) where

import Edge
import Directions


type Piece = Directions Edge64
