{-# LANGUAGE ScopedTypeVariables #-}

module FillGraph
  ( fillGraph
  ) where

import qualified Data.Set as Set


fillGraph :: forall a. (Ord a) =>(a -> [a]) -> a ->  Set.Set a
fillGraph neighbours start = go [start] Set.empty Set.empty
  where
    go :: [a] -> Set.Set a -> Set.Set a -> Set.Set a
    go [] inSet doneSet = inSet
    go (work:workList) inSet doneSet =
      if work `Set.member` doneSet
      then go workList inSet doneSet
      else go (neighbours work ++ workList)
             (Set.insert work inSet) (Set.insert work doneSet)
