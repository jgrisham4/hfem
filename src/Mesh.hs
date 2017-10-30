module Mesh
( Mesh(..)
, elemNodeNums
, getMeshElements
, getMeshNodes
, generateMesh
) where

import Data.List (nub)

import Node
import Element

-- Type for a mesh
newtype Mesh e a = Mesh [e a] deriving (Show,Eq)

-- Function for getting elements from a mesh
getMeshElements :: (Floating a,Element e) => Mesh e a -> [e a]
getMeshElements (Mesh elements) = elements

-- Function for getting nodes from a mesh
getMeshNodes :: (Floating a,Eq a,Element e) => Mesh e a -> [Node a]
getMeshNodes (Mesh elements) = nub $ concatMap getElementNodes elements

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
generateMesh :: (Fractional a, Element e) => [a] -> [a] -> [Int] -> ([Node a] -> Int -> e a) -> Mesh e a
generateMesh xInitial xFinal npts elemConstructor = Mesh elements
  where
    dim = length xInitial
    nodeCoords = mapM (\(lo, hi, n) -> [lo + (fromIntegral v/fromIntegral (n-1)) * hi | v <- [0..(n-1)]]) (zip3 xInitial xFinal npts)
    nodes = [Node i (nodeCoords !! i) | i <- [0..(length nodeCoords - 1)]]
    numNodes = length nodes
    nelem = product $ map (\x -> x-1) npts
    elemNodes = [[nodes !! n | n <- nn] | nn <- elemNodeNums dim npts]
    elements = [elemConstructor (elemNodes !! i) i| i <- [0..(nelem-1)]]
