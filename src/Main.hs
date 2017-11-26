import           Advection1D
import           Basis
import           Element
import           Mesh
import           Node
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

main = do
  print $ computeJacobian lineElem linShpFcn [0.0 :: Double]
  print $ show quadElem
  print $ show $ boundaryElements mesh2D !! 2
  print $ map (fst . elemMatrices lineElem linShpFcn) [1,2,3]
