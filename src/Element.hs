module Element
( Quad(..)
, Tri(..)
) where

-- Element types
-- I'm not sure that the order is really necessary as it is stored in the basis
-- as well??
data Quad = Quad Int [Int] deriving (Show,Eq)
data Tri  = Tri Int [Int] deriving (Show,Eq)

-- Only going to implement quads for now

class ElementType e where
  computeJacobian :: (Floating a) => e -> [a] -> Int -> a
