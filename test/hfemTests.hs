import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.JenkinsXML (jenkinsXMLRunner)

import Basis
import Node

-- Main
main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [jenkinsXMLRunner]

-- Setting up tests
tests :: TestTree
tests = testGroup "Tests" [basisTests, nodeTests]

{-
-----------------------------------------------------------
Basis tests
-----------------------------------------------------------
-}

-- Creating a linear basis
linearLagrangeBasis = Basis.Lagrange 1
lagrangePsi         = Basis.psi linearLagrangeBasis
lagrangeDPsi xi i   = Basis.dpsidxi linearLagrangeBasis xi i 1
lagrangeD2Psi xi i  = Basis.dpsidxi linearLagrangeBasis xi i 2

-- Linear Lagrange basis tests
basisTests = testGroup "Basis Tests"
  [ testCase "Linear Lagrange  xi=-1, i=0" $ assertEqual "returns 1 for xi=-1, i=0" ( 1.0 :: Double) (lagrangePsi   (-1.0 :: Double) 0)
  , testCase "Linear Lagrange   xi=1, i=0" $ assertEqual "returns 0 for  xi=1, i=0" ( 0.0 :: Double) (lagrangePsi   ( 1.0 :: Double) 0)
  , testCase "Linear Lagrange  xi=-1, i=1" $ assertEqual "returns 0 for xi=-1, i=1" ( 0.0 :: Double) (lagrangePsi   (-1.0 :: Double) 1)
  , testCase "Linear Lagrange   xi=1, i=1" $ assertEqual "returns 1 for  xi=1, i=1" ( 1.0 :: Double) (lagrangePsi   ( 1.0 :: Double) 1)
  , testCase "Linear Lagrange   ddxi, i=0" $ assertEqual "returns -1/2 for i=0"     (-0.5 :: Double) (lagrangeDPsi  ( 0.0 :: Double) 0)
  , testCase "Linear Lagrange   ddxi, i=1" $ assertEqual "returns  1/2 for i=1"     ( 0.5 :: Double) (lagrangeDPsi  ( 0.0 :: Double) 1)
  , testCase "Linear Lagrange d2dxi2, i=0" $ assertEqual "returns 0 for i=0"        ( 0.0 :: Double) (lagrangeD2Psi ( 0.0 :: Double) 0)
  , testCase "Linear Lagrange d2dxi2, i=1" $ assertEqual "returns 0 for i=1"        ( 0.0 :: Double) (lagrangeD2Psi ( 0.0 :: Double) 1)
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

nodeTests = testGroup "Node Tests"
  [ testCase "1d Node number"      $ assertEqual "returns 1"      nodeNum1 (nodeNumber node1d)
  , testCase "2d Node number"      $ assertEqual "returns 10"     nodeNum2 (nodeNumber node2d)
  , testCase "1d Node coordinates" $ assertEqual "returns coords" nodeCoords1 (nodeCoordinates node1d)
  , testCase "2d Node coordinates" $ assertEqual "returns coords" nodeCoords2 (nodeCoordinates node2d)
  ]
