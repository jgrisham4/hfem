import Basis
import Node
import ShapeFcns
import Element

linBasis  = Lagrange 1
linShpFcn = TensorProduct linBasis 1
linShpFcn2 = TensorProduct linBasis 2
nodes     = [Node 0 [0.0], Node 1 [0.5]]
nodes2d = [Node 0 [-1.0, -1.0], Node 1 [1.0, -1.0], Node 2 [1.0, 1.0], Node 3 [-1.0, 1.0]]
lineElem  = Line nodes 0
quadElem = Quad nodes2d 0
someCoords = [0.0, 0.0]

main =
  print $ show $ computeJacobianDet lineElem linShpFcn [0.0 :: Double]
