import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.Ingredients.ListTests
import Test.Tasty.Runners.JenkinsXML (jenkinsXMLRunner)

import Basis

-- Creating a linear basis
linearLagrangeBasis = Basis.Lagrange 1
lagrangePsi = Basis.psi linearLagrangeBasis

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = [jenkinsXMLRunner]
--    ingredients = [listingTests, jenkinsXMLRunner]

-- Setting up tests
tests :: TestTree
tests = testGroup "Tests" [basisTests]

-- Linear Lagrange basis tests
basisTests = testGroup "Basis Tests"
  [ testCase "Left 0" $ assertEqual "returns 1 for xi = -1, i = 0" (1.0 :: Double) (lagrangePsi (-1.0 :: Double) 0)
  , testCase "Right 0" $ assertEqual "returns 0 for xi = 1, i = 0" (0.0 :: Double) (lagrangePsi (1.0 :: Double) 0)
  ]

--spec :: Spec
--spec = do
--  describe "Prelude.head" $ do
--    it "returns 1 for xi = -1, i = 0" $ do
--      Basis.psi linearLagrangeBasis (-1.0 :: Double) 0 `shouldBe` (1.0 :: Double)
--    it "return 0 for xi = -1, i = 1" $ do
--      Basis.psi linearLagrangeBasis (-1.0 :: Double) 1 `shouldBe` (0.0 :: Double)
--    it "returns 1 for xi = 1, i = 1" $ do
--      Basis.psi linearLagrangeBasis (1.0 :: Double) 1 `shouldBe` (1.0 :: Double)
--    it "returns 0 for xi = -1, i = 1" $ do
--      Basis.psi linearLagrangeBasis (1.0 :: Double) 0 `shouldBe` (0.0 :: Double)
