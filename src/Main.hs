import Basis
import Node
import ShapeFcns
import Element

linBasis  = Lagrange 1
linShpFcn = TensorProduct linBasis 1
nodes     = [Node 0 [0.0], Node 1 [0.5]]
lineElem  = Line nodes 0

main = do
  print $ show $ computeJacobianDet lineElem linShpFcn [0.0 :: Double]
