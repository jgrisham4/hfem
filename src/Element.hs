module Element
( Quad(..)
, Tri(..)
) where

import Numeric.LinearAlgebra

import Node
import ShapeFcns

-- Element types
data Quad s = Quad s [Node] Int deriving (Show,Eq)
data Tri  s = Tri  s [Node] Int deriving (Show,Eq)

-- Only going to implement quads for now

class ElementType e where
  getElementNumber :: (ShapeFcnType s)                => e s -> Int
  getElementOrder  :: (ShapeFcnType s)                => e s -> Int
  computeJacobian  :: (ShapeFcnType s,Fractional a)   => e s -> [a] -> Int -> a

--instance ElementType Quad where
--  getElementNumber (Quad _ _ elemNum) = elemNum
--  getElementOrder  (Quad shpFcn _ _) = getShapeFcnOrder shpFcn
--  computeJacobian  (Quad shpFcn nodes elemNum) = matA <> matB where
--      matA = fromLists $ [[]]
--      matB = fromLists $ 
