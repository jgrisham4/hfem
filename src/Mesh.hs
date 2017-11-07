module Mesh
( Mesh(..)
, elemNodeNums
, belemNodeNums
, getMeshElements
, getMeshNodes
, generateMesh
) where

import Data.List (nub)

import Node
import Element

-- Type for a mesh
-- Mesh [Elements] [boundaryElements]
data Mesh e a = Mesh { elements :: [e a]
                     , boundaryElements :: [[e a]]
                     } deriving (Show,Eq)

-- Function for getting elements from a mesh
getMeshElements :: (Floating a,Element e) => Mesh e a -> [e a]
getMeshElements (Mesh elems _) = elems

-- Function for getting the boundary elements from a mesh

-- Function for getting nodes from a mesh
getMeshNodes :: (Floating a,Eq a,Element e) => Mesh e a -> [Node a]
getMeshNodes (Mesh elems _) = nub $ concatMap getElementNodes elems

-- Function for getting the global node numbers for boundary elements
belemNodeNums :: Int -> [Int] -> [[[Int]]]
belemNodeNums 1 npts = [[[0]], [[last npts - 1]]]
belemNodeNums 2 npts = [left, right, lower, upper]
  where
    imax  = head npts
    jmax  = npts !! 1
    left  = [[j*imax, (j+1)*imax]                 | j <- [0..(jmax-2)]]
    right = [[j*imax+imax-1, (j+1)*imax+imax-1]   | j <- [0..(jmax-2)]]
    lower = [[i, i+1]                             | i <- [0..(imax-2)]]
    upper = [[(jmax-1)*imax+i, (jmax-1)*imax+i+1] | i <- [0..(imax-2)]]
belemNodeNums 3 npts = [left, right, lower, upper, front, back]
  where
    imax  = head npts
    jmax  = npts !! 1
    kmax  = npts !! 2
    left  = [[0]]
    right = [[0]]
    lower = [[0]]
    upper = [[0]]
    front = [[0]]
    back  = [[0]]

-- Function for getting global node numbers for each element
elemNodeNums :: Int -> [Int] -> [[Int]]
elemNodeNums 1 npts = [[i,i+1] | i <- [0..(imax-1)]]
  where
    imax = head npts
elemNodeNums 2 npts = concat [[[j*imax+i, j*imax+i+1, (j+1)*imax+i+1, (j+1)*imax+i] | i <- [0..(imax-2)]] | j <- [0..(jmax-2)]]
  where
    imax = head npts
    jmax = npts !! 1
elemNodeNums 3 npts = concat $ concat [[[[k*jmax*imax+j*imax+i,k*jmax*imax+j*imax+i+1,
  k*jmax*imax+(j+1)*imax+i+1,k*jmax*imax+(j+1)*imax+i,
  (k+1)*jmax*imax+j*imax+i,(k+1)*jmax*imax+j*imax+i+1,
  (k+1)*jmax*imax+(j+1)*imax+i+1,(k+1)*jmax*imax+(j+1)*imax+i]
  | i <- [0..(imax-2)]] | j <- [0..(jmax-2)]] | k <- [0..(kmax-2)]]
    where
      imax = head npts
      jmax = npts !! 1
      kmax = npts !! 2

-- Function for generating an n-dimensional mesh
--belemNodeNums :: Int -> [Int] -> [[[Int]]]
generateMesh :: (Fractional a, Element e) => [a] -> [a] -> [Int] -> ([Node a] -> Int -> e a) -> Mesh e a
generateMesh xInitial xFinal npts elemConstructor = Mesh elems belems
  where
    dim = length xInitial
    nodeCoords = map reverse $ mapM (\(lo, hi, n) -> [lo + (fromIntegral v/fromIntegral (n-1)) * hi | v <- [0..(n-1)]]) (zip3 xInitial xFinal npts)
    nodes = [Node i (nodeCoords !! i) | i <- [0..(length nodeCoords - 1)]]
    numNodes = length nodes
    nelem = product $ map (\x -> x-1) npts
    belemNodes = [[[nodes !! n | n <- belemCon] | belemCon <- bCon] | bCon <- belemNodeNums dim npts]
    elemNodes = [[nodes !! n | n <- nn] | nn <- elemNodeNums dim npts]
    elems = [elemConstructor (elemNodes !! i) i | i <- [0..(nelem-1)]]
    belems = [[elemConstructor (belemNodes !! bId !! eId) (eId + bId * length (belemNodes !! bId)) | eId <- [0..(length (belemNodes !! bId) - 1)]] | bId <- [0..(length belemNodes - 1)]]
