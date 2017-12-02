module Mesh
( Mesh(..)
, elemNodeNums
, belemNodeNums
, getMeshElements
, getMeshNodes
, generateMesh
, genDMesh
, nodesFromCoords
, discretizeSpace
, duplicateInterior
, disElemNodeNums
, writeMesh
) where

import           Data.List (nub)

import           Element
import           Node

-- Type for a mesh
-- Mesh [Elements] [boundaryElements]
data Mesh e a = Mesh { elements         :: [e a]
                     , boundaryElements :: [[e a]]
                     } deriving (Show,Eq)

-- Function for getting elements from a mesh
getMeshElements :: (Fractional a,Element e) => Mesh e a -> [e a]
getMeshElements (Mesh elems _) = elems

-- Function for getting the boundary elements from a mesh

-- Function for getting nodes from a mesh
getMeshNodes :: (Fractional a,Eq a,Element e) => Mesh e a -> [Node a]
getMeshNodes (Mesh elems _) = nub $ concatMap getElementNodes elems

-- Writes a mesh to a file.
writeMesh :: (Show a,Fractional a,Eq a,Element e) => Mesh e a -> String -> IO()
writeMesh grid fname = let
  header = show ((length . getMeshNodes) grid) ++ " " ++ show ((length . getMeshElements) grid) ++ "\n"
  nodeStrs = concatMap (\x -> (show . head . nodeCoordinates) x ++ "\n") (getMeshNodes grid)
  connStrs = (concat . concat) $ map (reverse . mappend ["\n"] . reverse) $ (fmap . fmap) (\x -> (show . nodeNumber) x ++ " ") $ map getElementNodes (getMeshElements grid)
  gridStr = header ++ nodeStrs ++ connStrs
  in writeFile fname gridStr

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

-- Function for building nodes from coordinates
nodesFromCoords :: Fractional a => [[a]] -> [Node a]
nodesFromCoords coords = zipWith Node [0..(length coords - 1)] coords

-- Function for discretizing some space
discretizeSpace :: Fractional a => [a] -> [a] -> [Int] -> [[a]]
discretizeSpace xi xf n = map reverse $ mapM (\(lo, hi, n) -> [lo + (fromIntegral v/fromIntegral (n-1)) * hi | v <- [0..(n-1)]]) (zip3 xi xf n)

-- Function for generating an n-dimensional mesh
--belemNodeNums :: Int -> [Int] -> [[[Int]]]
generateMesh :: (Fractional a, Element e) => [a] -> [a] -> [Int] -> ([Node a] -> Int -> e a) -> Mesh e a
generateMesh xInitial xFinal npts elemConstructor = Mesh elems belems
  where
    dim = length xInitial
    nodes = nodesFromCoords $ discretizeSpace xInitial xFinal npts
    nelem = (product . (map (subtract 1))) npts
    belemNodes = [[[nodes !! n | n <- belemCon] | belemCon <- bCon] | bCon <- belemNodeNums dim npts]
    elemNodes = [[nodes !! n | n <- nn] | nn <- elemNodeNums dim npts]
    elems = [elemConstructor (elemNodes !! i) i | i <- [0..(nelem-1)]]
    belems = [[elemConstructor (belemNodes !! bId !! eId) (eId + bId * length (belemNodes !! bId)) | eId <- [0..(length (belemNodes !! bId) - 1)]] | bId <- [0..(length belemNodes - 1)]]

-- Function for duplicating interior coordinates
-- Only works for 1d for now.
duplicateInterior :: [[a]] -> [[a]]
duplicateInterior c = head c : (concatMap (replicate 2) ((init . tail) c)) ++ [last c]

-- Element node numbers -- 1D
elemNodeNums1d :: Int -> [[Int]]
elemNodeNums1d nn = [[elemnn !! 2*i, elemnn !! 2*i+1] | i <- [0..nn-1]]
  where
    elemnn = concat $ duplicateInterior $ map (:[]) [0..nn - 1]

disElemNodeNums :: Int -> [[Int]]
disElemNodeNums nn = [[i,i+1] | i <- [0,2..(2*(nn-1)-1)]]

-- Function for generating a discontinuous mesh
genDMesh :: (Fractional a, Element e) => a -> a -> Int -> ([Node a] -> Int -> e a) -> Mesh e a
genDMesh xi xf n elemDataCtor = Mesh elems belems
  where
    nodes = nodesFromCoords . duplicateInterior $ discretizeSpace [xi] [xf] [n]
    nelem = subtract 1 n
    elemNodes = [[nodes !! k | k <- nn] | nn <- disElemNodeNums n]
    elems = [elemDataCtor (elemNodes !! i) i | i <- [0..(nelem-1)]]
    belems = [[elemDataCtor [head nodes] 0], [elemDataCtor [nodes !! (n-1)] 1]]
