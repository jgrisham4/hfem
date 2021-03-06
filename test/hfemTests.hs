import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners.JenkinsXML (jenkinsXMLRunner)

import           Basis
import qualified Element
import           Mesh
import           Node
import           ShapeFcns


-- Main
main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [jenkinsXMLRunner]

-- Setting up tests
tests :: TestTree
tests = testGroup "Tests" [basisTests, shpFcnTests, shpFcnBasisMapTests, shpFcnDerivTests, nodeTests, elemTests, meshTests]

-----------------------------------------------------------
-- Basis tests
-----------------------------------------------------------

-- Creating a linear basis
linLag = Lagrange 1

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

-----------------------------------------------------------
-- Shape function tests
-----------------------------------------------------------

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
  [ testCase "1D Shape fcn to basis map 0" $ assertEqual "returns [0]"     [0]     (shpFcnBasisMap 1 1 0)
  , testCase "1D Shape fcn to basis map 1" $ assertEqual "returns [1]"     [1]     (shpFcnBasisMap 1 1 1)
  , testCase "2D Shape fcn to basis map 0" $ assertEqual "returns [0,0]"   [0,0]   (shpFcnBasisMap 2 1 0)
  , testCase "2D Shape fcn to basis map 1" $ assertEqual "returns [1,0]"   [1,0]   (shpFcnBasisMap 2 1 1)
  , testCase "2D Shape fcn to basis map 2" $ assertEqual "returns [1,1]"   [1,1]   (shpFcnBasisMap 2 1 2)
  , testCase "2D Shape fcn to basis map 3" $ assertEqual "returns [0,1]"   [0,1]   (shpFcnBasisMap 2 1 3)
  , testCase "3D Shape fcn to basis map 0" $ assertEqual "returns [0,0,0]" [0,0,0] (shpFcnBasisMap 3 1 0)
  , testCase "3D Shape fcn to basis map 1" $ assertEqual "returns [1,0,0]" [1,0,0] (shpFcnBasisMap 3 1 1)
  , testCase "3D Shape fcn to basis map 2" $ assertEqual "returns [1,1,0]" [1,1,0] (shpFcnBasisMap 3 1 2)
  , testCase "3D Shape fcn to basis map 3" $ assertEqual "returns [0,1,0]" [0,1,0] (shpFcnBasisMap 3 1 3)
  , testCase "3D Shape fcn to basis map 4" $ assertEqual "returns [0,0,1]" [0,0,1] (shpFcnBasisMap 3 1 4)
  , testCase "3D Shape fcn to basis map 5" $ assertEqual "returns [1,0,1]" [1,0,1] (shpFcnBasisMap 3 1 5)
  , testCase "3D Shape fcn to basis map 6" $ assertEqual "returns [1,1,1]" [1,1,1] (shpFcnBasisMap 3 1 6)
  , testCase "3D Shape fcn to basis map 7" $ assertEqual "returns [0,1,1]" [0,1,1] (shpFcnBasisMap 3 1 7)
  ]

-- Creating some tensoir product shape functions
tp1 = TensorProduct linLag 1
tp2 = TensorProduct linLag 2
tp3 = TensorProduct linLag 3

-- Hard coded shape function definition used to check against
shpFcn2d :: (Floating a) => [a] -> Int -> [Int] -> a
shpFcn2d coords nodeNum derivs = psi linLag (head coords) (head basisNum) (head derivs) * psi linLag (coords !! 1) (basisNum !! 1) (derivs !! 1)
  where
    basisNum = shpFcnBasisMap 2 1 nodeNum
shpFcn3d :: (Floating a) => [a] -> Int -> [Int] -> a
shpFcn3d coords nodeNum derivs = psi linLag (head coords) (head basisNum) (head derivs) * psi linLag (coords !! 1) (basisNum !! 1) (derivs !! 1) * psi linLag (coords !! 2) (basisNum !! 2) (derivs !! 2)
  where
    basisNum = shpFcnBasisMap 3 1 nodeNum

-- Some arbitrary points
some2dPoint = [0.25, -0.3]
some3dPoint = [-0.6, 1.0, 0.3]

shpFcnDerivTests = testGroup "Shape function derivative tests"
  [ testCase "1D Shape fcn deriv"   $ assertEqual "returns [-1/2]" (psi linLag 0.0 0 1) (n tp1 [0.0] 0 [1])
  , testCase "1D Shape fcn deriv"   $ assertEqual "returns [ 1/2]" (psi linLag 0.0 1 1) (n tp1 [0.0] 1 [1])
  , testCase "2D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 0 [1,0]) (n tp2 some2dPoint 0 [1,0])
  , testCase "2D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 0 [0,1]) (n tp2 some2dPoint 0 [0,1])
  , testCase "2D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 0 [1,1]) (n tp2 some2dPoint 0 [1,1])
  , testCase "2D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 0 [2,0]) (n tp2 some2dPoint 0 [2,0])
  , testCase "2D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 1 [1,0]) (n tp2 some2dPoint 1 [1,0])
  , testCase "2D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 1 [0,1]) (n tp2 some2dPoint 1 [0,1])
  , testCase "2D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 1 [1,1]) (n tp2 some2dPoint 1 [1,1])
  , testCase "2D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 1 [2,0]) (n tp2 some2dPoint 1 [2,0])
  , testCase "2D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 2 [1,0]) (n tp2 some2dPoint 2 [1,0])
  , testCase "2D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 2 [0,1]) (n tp2 some2dPoint 2 [0,1])
  , testCase "2D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 2 [1,1]) (n tp2 some2dPoint 2 [1,1])
  , testCase "2D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 2 [2,0]) (n tp2 some2dPoint 2 [2,0])
  , testCase "2D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 3 [1,0]) (n tp2 some2dPoint 3 [1,0])
  , testCase "2D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 3 [0,1]) (n tp2 some2dPoint 3 [0,1])
  , testCase "2D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 3 [1,1]) (n tp2 some2dPoint 3 [1,1])
  , testCase "2D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn2d some2dPoint 3 [2,0]) (n tp2 some2dPoint 3 [2,0])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [1,0,0]) (n tp3 some3dPoint 0 [1,0,0])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [0,1,0]) (n tp3 some3dPoint 0 [0,1,0])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [0,0,1]) (n tp3 some3dPoint 0 [0,0,1])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [0,1,1]) (n tp3 some3dPoint 0 [0,1,1])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [1,0,1]) (n tp3 some3dPoint 0 [1,0,1])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [1,1,0]) (n tp3 some3dPoint 0 [1,1,0])
  , testCase "3D Shape fcn deriv 0" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 0 [2,1,0]) (n tp3 some3dPoint 0 [2,1,0])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [1,0,0]) (n tp3 some3dPoint 1 [1,0,0])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [0,1,0]) (n tp3 some3dPoint 1 [0,1,0])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [0,0,1]) (n tp3 some3dPoint 1 [0,0,1])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [0,1,1]) (n tp3 some3dPoint 1 [0,1,1])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [1,0,1]) (n tp3 some3dPoint 1 [1,0,1])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [1,1,0]) (n tp3 some3dPoint 1 [1,1,0])
  , testCase "3D Shape fcn deriv 1" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 1 [2,1,0]) (n tp3 some3dPoint 1 [2,1,0])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [1,0,0]) (n tp3 some3dPoint 2 [1,0,0])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [0,1,0]) (n tp3 some3dPoint 2 [0,1,0])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [0,0,1]) (n tp3 some3dPoint 2 [0,0,1])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [0,1,1]) (n tp3 some3dPoint 2 [0,1,1])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [1,0,1]) (n tp3 some3dPoint 2 [1,0,1])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [1,1,0]) (n tp3 some3dPoint 2 [1,1,0])
  , testCase "3D Shape fcn deriv 2" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 2 [2,1,0]) (n tp3 some3dPoint 2 [2,1,0])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [1,0,0]) (n tp3 some3dPoint 3 [1,0,0])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [0,1,0]) (n tp3 some3dPoint 3 [0,1,0])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [0,0,1]) (n tp3 some3dPoint 3 [0,0,1])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [0,1,1]) (n tp3 some3dPoint 3 [0,1,1])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [1,0,1]) (n tp3 some3dPoint 3 [1,0,1])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [1,1,0]) (n tp3 some3dPoint 3 [1,1,0])
  , testCase "3D Shape fcn deriv 3" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 3 [2,1,0]) (n tp3 some3dPoint 3 [2,1,0])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [1,0,0]) (n tp3 some3dPoint 4 [1,0,0])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [0,1,0]) (n tp3 some3dPoint 4 [0,1,0])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [0,0,1]) (n tp3 some3dPoint 4 [0,0,1])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [0,1,1]) (n tp3 some3dPoint 4 [0,1,1])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [1,0,1]) (n tp3 some3dPoint 4 [1,0,1])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [1,1,0]) (n tp3 some3dPoint 4 [1,1,0])
  , testCase "3D Shape fcn deriv 4" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 4 [2,1,0]) (n tp3 some3dPoint 4 [2,1,0])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [1,0,0]) (n tp3 some3dPoint 5 [1,0,0])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [0,1,0]) (n tp3 some3dPoint 5 [0,1,0])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [0,0,1]) (n tp3 some3dPoint 5 [0,0,1])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [0,1,1]) (n tp3 some3dPoint 5 [0,1,1])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [1,0,1]) (n tp3 some3dPoint 5 [1,0,1])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [1,1,0]) (n tp3 some3dPoint 5 [1,1,0])
  , testCase "3D Shape fcn deriv 5" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 5 [2,1,0]) (n tp3 some3dPoint 5 [2,1,0])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [1,0,0]) (n tp3 some3dPoint 6 [1,0,0])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [0,1,0]) (n tp3 some3dPoint 6 [0,1,0])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [0,0,1]) (n tp3 some3dPoint 6 [0,0,1])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [0,1,1]) (n tp3 some3dPoint 6 [0,1,1])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [1,0,1]) (n tp3 some3dPoint 6 [1,0,1])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [1,1,0]) (n tp3 some3dPoint 6 [1,1,0])
  , testCase "3D Shape fcn deriv 6" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 6 [2,1,0]) (n tp3 some3dPoint 6 [2,1,0])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [1,0,0]) (n tp3 some3dPoint 7 [1,0,0])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [0,1,0]) (n tp3 some3dPoint 7 [0,1,0])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [0,0,1]) (n tp3 some3dPoint 7 [0,0,1])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [0,1,1]) (n tp3 some3dPoint 7 [0,1,1])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [1,0,1]) (n tp3 some3dPoint 7 [1,0,1])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [1,1,0]) (n tp3 some3dPoint 7 [1,1,0])
  , testCase "3D Shape fcn deriv 7" $ assertEqual "returns same as hand-coded version" (shpFcn3d some3dPoint 7 [2,1,0]) (n tp3 some3dPoint 7 [2,1,0])
  ]

-----------------------------------------------------------
-- Node tests
-----------------------------------------------------------

-- Some inputs
nodeNum1    = 1 :: Int
nodeNum2    = 10 :: Int
nodeNum3    = 5000010 :: Int
nodeCoords1 = [0.25 :: Double]
nodeCoords2 = [1.0 :: Double, 2.0 :: Double]
nodeCoords3 = [1000010.0 :: Double, 5.0 :: Double, -1234124.0 :: Double]

-- Creating a node
node1d = Node nodeNum1 nodeCoords1
node2d = Node nodeNum2 nodeCoords2
node3d = Node nodeNum3 nodeCoords3

nodeTests = testGroup "Node tests"
  [ testCase "1d Node number"      $ assertEqual "returns 1"       nodeNum1    (nodeNumber node1d)
  , testCase "2d Node number"      $ assertEqual "returns 10"      nodeNum2    (nodeNumber node2d)
  , testCase "3d Node number"      $ assertEqual "returns 5000010" nodeNum3    (nodeNumber node3d)
  , testCase "1d Node coordinates" $ assertEqual "returns coords"  nodeCoords1 (nodeCoordinates node1d)
  , testCase "2d Node coordinates" $ assertEqual "returns coords"  nodeCoords2 (nodeCoordinates node2d)
  , testCase "3d Node coordinates" $ assertEqual "returns coords"  nodeCoords3 (nodeCoordinates node3d)
  ]

-----------------------------------------------------------
-- Element tests
-----------------------------------------------------------

-- Creating a 1D element
nodes1d = [Node 0 [-1.0], Node 1 [1.0]]
nodes2d = [Node 0 [-1.0, -1.0], Node 1 [1.0, -1.0], Node 2 [1.0, 1.0], Node 3 [-1.0, 1.0]]
nodes2d' = [Node 0 [0.0, 0.0], Node 1 [1.0, 0.0], Node 2 [1.0, 1.0], Node 3 [0.0, 1.0]]
nodes3d = [Node 0 [-1.0,-1.0,-1.0], Node 1 [1.0,-1.0,-1.0], Node 2 [1.0,1.0,-1.0], Node 3 [-1.0,1.0,-1.0],Node 4 [-1.0,-1.0,1.0], Node 5 [1.0,-1.0,1.0], Node 6 [1.0,1.0,1.0], Node 7 [-1.0,1.0,1.0]]
nodes3d' = [Node 0 [0.0,0.0,0.0], Node 1 [1.0,0.0,0.0], Node 2 [1.0,1.0,0.0], Node 3 [0.0,1.0,0.0],Node 4 [0.0,0.0,1.0], Node 5 [1.0,0.0,1.0], Node 6 [1.0,1.0,1.0], Node 7 [0.0,1.0,1.0]]
lineElem = Element.StructElem nodes1d 0
quadElem = Element.StructElem nodes2d 0
quadElem' = Element.StructElem nodes2d' 0
hexElem = Element.StructElem nodes3d 0
hexElem' = Element.StructElem nodes3d' 0

elemTests = testGroup "Element tests"
  [ testCase "1d element number"       $ assertEqual "returns 0"    (Element.getElementNumber lineElem) (0 :: Int)
  , testCase "1d Jacobian Determinant" $ assertEqual "returns 1"    (Element.computeJacobianDet lineElem tp1 [0.0 :: Double]) (1.0 :: Double)
  , testCase "2d element number"       $ assertEqual "returns 0"    (Element.getElementNumber quadElem) (0 :: Int)
  , testCase "2d Jacobian Determinant" $ assertEqual "returns 1"    (Element.computeJacobianDet quadElem tp2 [0.0 :: Double, 0.0 :: Double]) (1.0 :: Double)
  , testCase "2d Jacobian Determinant" $ assertEqual "returns 0.25" (Element.computeJacobianDet quadElem' tp2 [0.0 :: Double, 0.0 :: Double]) (0.25 :: Double)
  , testCase "3d Jacobian Determinant" $ assertEqual "returns 1"    (Element.computeJacobianDet hexElem tp3 [0.0,0.0,0.0]) (1.0::Double)
  , testCase "3d Jacobian Determinant" $ assertEqual "returns 1/8"  (Element.computeJacobianDet hexElem' tp3 [0.0,0.0,0.0]) (1.0 / 8.0 :: Double)
  ]

-----------------------------------------------------------
-- Mesh tests
-----------------------------------------------------------

mesh1d = generateMesh [0.0 :: Double] [1.0 :: Double] [3] Element.StructElem
mesh1dElems = getMeshElements mesh1d
mesh1dNodes = getMeshNodes mesh1d
mesh2d = generateMesh (replicate 2 (0.0::Double)) (replicate 2 (1.0::Double)) [3,3] Element.StructElem
mesh2dElems = getMeshElements mesh2d
leftBndNodes = map (map nodeNumber . Element.getElementNodes) (head $ boundaryElements mesh2d)

meshTests = testGroup "Mesh tests"
  [ testCase "1d element numbers"      $ assertEqual "returns [0,1]" [0,1] (map Element.getElementNumber mesh1dElems)
  , testCase "1d node numbers"         $ assertEqual "returns [0.0, 0.5, 1.0]" [0.0::Double, 0.5::Double, 1.0::Double] (concatMap nodeCoordinates mesh1dNodes)
  , testCase "1d boundary elements"    $ assertEqual "returns boundary elements" [[Element.StructElem [head mesh1dNodes] 0],[Element.StructElem [last mesh1dNodes] 1]] (boundaryElements mesh1d)
  , testCase "2d element connectivity" $ assertEqual "returns first elem con" [0,1,4,3] (Element.getConnectivity (head mesh2dElems))
  , testCase "2d element connectivity" $ assertEqual "returns last elem con" [4,5,8,7] (Element.getConnectivity (last mesh2dElems))
  , testCase "2d boundary elements"    $ assertEqual "returns boundary elements" [[0,3],[3,6]] leftBndNodes
  ]

