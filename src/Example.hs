module Example
( Node(..)
, Line2D(..)
, Line3D(..)
) where

-- Type for a node which is part of an element
data Node a = Node {
  nodeNumber      :: Int,
  nodeCoordinates :: [a]
} deriving (Show,Eq)

data Spline = Linear | Quadratic | Cubic

data Line2D a = Line2D [Node a]
data Line2DSplined s a = Line3D s [Node a]

class Line l where
  getNodes 
