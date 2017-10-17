module Node
( Node(..)
) where

-- Type for a node which is part of an element
data Node a = Node {
  nodeNumber      :: Int,
  nodeCoordinates :: [a]
} deriving (Show,Eq)
