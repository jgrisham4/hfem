module Node
( Node(..)
) where

-- Data type for a node which is part of an element
data Node a = Node {
  nodeNumber      :: Int,
  nodeCoordinates :: [a]
} deriving (Show,Eq)
