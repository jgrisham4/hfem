module Mesh
( generateNDPoints
) where

import Node
import Element

data Mesh = Mesh [Element]

-- Function for generating coordinates for an N-dimensional grid
generateNDPoints :: (Floating a) => [a] -> [a] -> [Int] -> [[a]]
generateNDPoints minVals maxVals nIncr = mapM (\(lo, hi, n) -> [lo + (fromIntegral v/fromIntegral (n-1)) * hi | v <- [0..(n-1)]]) (zip3 minVals maxVals nIncr)

-- Function for generating n-dimensional uniform meshes
--generateUniformMesh
