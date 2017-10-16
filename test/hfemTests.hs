import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.JenkinsXML (jenkinsXMLRunner)

import Basis
import ShapeFcns
import Node


-- Main
main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [jenkinsXMLRunner]

-- Setting up tests
tests :: TestTree
tests = testGroup "Tests" [basisTests, shpFcnTests, shpFcnBasisMapTests, shpFcnDerivTests, nodeTests]

{-
-----------------------------------------------------------
Basis tests
-----------------------------------------------------------
-}

-- Creating a linear basis
linLag = Basis.Lagrange 1

-- Linear Lagrange basis tests
basisTests = testGroup "Basis tests"
  [ testCase "Linear Lagrange  xi=-1, i=0" $ assertEqual "returns 1 for xi=-1, i=0" ( 1.0 :: Double) (psi linLag (-1.0 :: Double) 0 0)
  , testCase "Linear Lagrange   xi=1, i=0" $ assertEqual "returns 0 for  xi=1, i=0" ( 0.0 :: Double) (psi linLag ( 1.0 :: Double) 0 0)
  , testCase "Linear Lagrange  xi=-1, i=1" $ assertEqual "returns 0 for xi=-1, i=1" ( 0.0 :: Double) (psi linLag (-1.0 :: Double) 1 0)
  , testCase "Linear Lagrange   xi=1, i=1" $ assertEqual "returns 1 for  xi=1, i=1" ( 1.0 :: Double) (psi linLag ( 1.0 :: Double) 1 0)
  , testCase "Linear Lagrange   ddxi, i=0" $ assertEqual "returns -1/2 for i=0"     (-0.5 :: Double) (psi linLag ( 0.0 :: Double) 0 1)
  , testCase "Linear Lagrange   ddxi, i=1" $ assertEqual "returns  1/2 for i=1"     ( 0.5 :: Double) (psi linLag ( 0.0 :: Double) 1 1)
  , testCase "Linear Lagrange d2dxi2, i=0" $ assertEqual "returns 0 for i=0"        ( 0.0 :: Double) (psi linLag ( 0.0 :: Double) 0 2)
  , testCase "Linear Lagrange d2dxi2, i=1" $ assertEqual "returns 0 for i=1"        ( 0.0 :: Double) (psi linLag ( 0.0 :: Double) 1 2)
  ]

{-
-----------------------------------------------------------
Shape function tests
-----------------------------------------------------------
-}

tPShpFcn = TensorProduct linLag 2

shpFcnTests = testGroup "Shape function tests"
  [ testCase "Lagrange tensor prod getBasis" $ assertEqual "returns Lagrange 1" linLag (getBasis tPShpFcn)
  , testCase "Lagrange tensor prod 0, i=0"   $ assertEqual "returns 1 for xi=-1, eta=-1, i=0" 1.0 (n tPShpFcn [-1.0,-1.0] 0 [0,0])
  , testCase "Lagrange tensor prod 1, i=0"   $ assertEqual "returns 0 for xi= 1, eta=-1, i=0" 0.0 (n tPShpFcn [ 1.0,-1.0] 0 [0,0])
  , testCase "Lagrange tensor prod 2, i=0"   $ assertEqual "returns 0 for xi= 1, eta= 1, i=0" 0.0 (n tPShpFcn [ 1.0, 1.0] 0 [0,0])
  , testCase "Lagrange tensor prod 3, i=0"   $ assertEqual "returns 0 for xi=-1, eta= 1, i=0" 0.0 (n tPShpFcn [-1.0, 1.0] 0 [0,0])
  , testCase "Lagrange tensor prod 0, i=1"   $ assertEqual "returns 0 for xi=-1, eta=-1, i=1" 0.0 (n tPShpFcn [-1.0,-1.0] 1 [0,0])
  , testCase "Lagrange tensor prod 1, i=1"   $ assertEqual "returns 1 for xi= 1, eta=-1, i=1" 1.0 (n tPShpFcn [ 1.0,-1.0] 1 [0,0])
  , testCase "Lagrange tensor prod 2, i=1"   $ assertEqual "returns 0 for xi= 1, eta= 1, i=1" 0.0 (n tPShpFcn [ 1.0, 1.0] 1 [0,0])
  , testCase "Lagrange tensor prod 3, i=1"   $ assertEqual "returns 0 for xi=-1, eta= 1, i=1" 0.0 (n tPShpFcn [-1.0, 1.0] 1 [0,0])
  , testCase "Lagrange tensor prod 0, i=2"   $ assertEqual "returns 0 for xi=-1, eta=-1, i=2" 0.0 (n tPShpFcn [-1.0,-1.0] 2 [0,0])
  , testCase "Lagrange tensor prod 1, i=2"   $ assertEqual "returns 0 for xi= 1, eta=-1, i=2" 0.0 (n tPShpFcn [ 1.0,-1.0] 2 [0,0])
  , testCase "Lagrange tensor prod 2, i=2"   $ assertEqual "returns 1 for xi= 1, eta= 1, i=2" 1.0 (n tPShpFcn [ 1.0, 1.0] 2 [0,0])
  , testCase "Lagrange tensor prod 3, i=2"   $ assertEqual "returns 0 for xi=-1, eta= 1, i=2" 0.0 (n tPShpFcn [-1.0, 1.0] 2 [0,0])
  , testCase "Lagrange tensor prod 0, i=3"   $ assertEqual "returns 0 for xi=-1, eta=-1, i=3" 0.0 (n tPShpFcn [-1.0,-1.0] 3 [0,0])
  , testCase "Lagrange tensor prod 1, i=3"   $ assertEqual "returns 0 for xi= 1, eta=-1, i=3" 0.0 (n tPShpFcn [ 1.0,-1.0] 3 [0,0])
  , testCase "Lagrange tensor prod 2, i=3"   $ assertEqual "returns 0 for xi= 1, eta= 1, i=3" 0.0 (n tPShpFcn [ 1.0, 1.0] 3 [0,0])
  , testCase "Lagrange tensor prod 3, i=3"   $ assertEqual "returns 1 for xi=-1, eta= 1, i=3" 1.0 (n tPShpFcn [-1.0, 1.0] 3 [0,0])
  ]

shpFcnBasisMapTests = testGroup "Shape function-basis mapping tests"
  [ testCase "1D Shape fcn to basis map 0" $ assertEqual "returns [0]"     [0]     (shpFcnBasisMap 0 1)
  , testCase "1D Shape fcn to basis map 1" $ assertEqual "returns [1]"     [1]     (shpFcnBasisMap 1 1)
  , testCase "2D Shape fcn to basis map 0" $ assertEqual "returns [0,0]"   [0,0]   (shpFcnBasisMap 0 2)
  , testCase "2D Shape fcn to basis map 1" $ assertEqual "returns [1,0]"   [1,0]   (shpFcnBasisMap 1 2)
  , testCase "2D Shape fcn to basis map 2" $ assertEqual "returns [1,1]"   [1,1]   (shpFcnBasisMap 2 2)
  , testCase "2D Shape fcn to basis map 3" $ assertEqual "returns [0,1]"   [0,1]   (shpFcnBasisMap 3 2)
  , testCase "3D Shape fcn to basis map 0" $ assertEqual "returns [0,0,0]" [0,0,0] (shpFcnBasisMap 0 3)
  , testCase "3D Shape fcn to basis map 1" $ assertEqual "returns [1,0,0]" [1,0,0] (shpFcnBasisMap 1 3)
  , testCase "3D Shape fcn to basis map 2" $ assertEqual "returns [1,1,0]" [1,1,0] (shpFcnBasisMap 2 3)
  , testCase "3D Shape fcn to basis map 3" $ assertEqual "returns [0,1,0]" [0,1,0] (shpFcnBasisMap 3 3)
  , testCase "3D Shape fcn to basis map 4" $ assertEqual "returns [0,0,1]" [0,0,1] (shpFcnBasisMap 4 3)
  , testCase "3D Shape fcn to basis map 5" $ assertEqual "returns [1,0,1]" [1,0,1] (shpFcnBasisMap 5 3)
  , testCase "3D Shape fcn to basis map 6" $ assertEqual "returns [1,1,1]" [1,1,1] (shpFcnBasisMap 6 3)
  , testCase "3D Shape fcn to basis map 7" $ assertEqual "returns [0,1,1]" [0,1,1] (shpFcnBasisMap 7 3)
  ]

tp1 = TensorProduct linLag 1

shpFcnDerivTests = testGroup "Shape function derivative tests"
  [ testCase "1D Shape fcn deriv" $ assertEqual "returns [-1/2]" (psi linLag 0.0 0 1) (n tp1 [0.0] 0 [1])
  , testCase "1D Shape fcn deriv" $ assertEqual "returns [ 1/2]" (psi linLag 0.0 1 1) (n tp1 [0.0] 1 [1])
  ]

{-
-----------------------------------------------------------
Node tests
-----------------------------------------------------------
-}

-- Some inputs
nodeNum1    = 1 :: Int
nodeNum2    = 10 :: Int
nodeCoords1 = [0.25 :: Double]
nodeCoords2 = [1.0 :: Double, 2.0 :: Double]

-- Creating a node
node1d = Node nodeNum1 nodeCoords1
node2d = Node nodeNum2 nodeCoords2

nodeTests = testGroup "Node tests"
  [ testCase "1d Node number"      $ assertEqual "returns 1"      nodeNum1 (nodeNumber node1d)
  , testCase "2d Node number"      $ assertEqual "returns 10"     nodeNum2 (nodeNumber node2d)
  , testCase "1d Node coordinates" $ assertEqual "returns coords" nodeCoords1 (nodeCoordinates node1d)
  , testCase "2d Node coordinates" $ assertEqual "returns coords" nodeCoords2 (nodeCoordinates node2d)
  ]

{-
-----------------------------------------------------------
Element tests
-----------------------------------------------------------
-}
