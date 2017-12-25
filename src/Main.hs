import           Advection1D
import           Basis
import           Element
import           Mesh
import           Node
import           Numeric.LinearAlgebra.HMatrix as HMat
import           Projection
import           ShapeFcns

linBasis   = Lagrange 1
linShpFcn  = TensorProduct linBasis 1
linShpFcn2 = TensorProduct linBasis 2
nodes      = [Node 0 [0.0], Node 1 [0.25]]
nodes2d    = [Node 0 [-1.0, -1.0], Node 1 [1.0, -1.0], Node 2 [1.0, 1.0], Node 3 [-1.0, 1.0]]
lineElem   = StructElem nodes 0
quadElem   = StructElem nodes2d 0
someCoords = [0.0, 0.0]
mesh2D     = generateMesh [0.0, 0.0] [1.0, 1.0] [3, 3] StructElem
mesh1D     = generateMesh [0.0] [1.0] [6] StructElem
meshDiscon = genDMesh (0.0::Double) (3.0::Double) 10 StructElem
lineElemMats = elemMatrices lineElem linShpFcn 1
globalMats = assembleGlobalMatrices meshDiscon linShpFcn 1 1.0

someFunction :: [Double] -> Double
someFunction [x] = sin x

nodalInitialConds = map (\e -> projectL2 e linShpFcn 2 someFunction) (getMeshElements meshDiscon) -- Should be [Matrix Double]
--nodeCs = map (getElementNodes) (getMeshElements meshDiscon)
--dispElemIC :: [Double] -> [Maybe (Matrix Double)] -> String
--dispElemIC nodeCoords nodeICs = [show (nodeCoords !! i) ++ " " ++ show (atIndex nodeICs (i,0)) ++ "\n" | i <- [0..1]]
--icStrings = (concat . concat) (zipWith dispElemIC nodeCs nodalInitialConds)

main = do
  print $ computeJacobian lineElem linShpFcn [0.0 :: Double]
  print $ show quadElem
  print $ show $ boundaryElements mesh2D !! 2
  print $ map (fst . elemMatrices lineElem linShpFcn) [1,2,3]
  writeMesh meshDiscon "mesh.dat"
  print $ HMat.toDense $ head globalMats
  print $ HMat.toDense $ globalMats !! 1
  print $ HMat.toDense $ last globalMats
  --writeFile "sine_ic.dat" icStrings
