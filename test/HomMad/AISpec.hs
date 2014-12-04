module HomMad.AISpec where

import Test.Hspec
import HomMad.Goban
import HomMad.AI

spec :: Spec
spec = do
  let testStatusA = foldl putStone initGame $ map coord [
                     (3, 3) -- B
                    ,(1, 3) -- W
                    ,(3, 4) -- B
                    ,(1, 5) -- W
                    ,(4, 3) -- B
                    ,(1, 6) -- W
                    ,(4, 4) -- B
                    ,(1, 8) -- W
                    ,(4, 5) -- B
                    ,(2, 3) -- W
                    ,(5, 4) -- B
                    ,(2, 4) -- W
                    ,(5, 6) -- B
                    ,(2, 5) -- W
                    ,(6, 4) -- B
                    ,(2, 7) -- W
                    ,(6, 5) -- B
                    ,(2, 8) -- W
                    ,(6, 6) -- B
                    ,(2, 9) -- W
                    ,(7, 1) -- B
                    ,(8, 9) -- W
                    ,(7, 2) -- B
                    ,(9, 8) -- W
                    ,(7, 3) -- B
                    ,(4, 9) -- W
                    ,(8, 1) -- B
                    ,(5, 9) -- W
                    ,(8, 3) -- B
                    ,(6, 9) -- W
                    ,(9, 2) -- B
                    ]

  describe "isEye" $ do
    it "is eye of black" $ do
      isEye (pass testStatusA) (coord (5,5)) `shouldBe` True
    it "is not eye of white" $ do
      isEye testStatusA (coord (5,5)) `shouldBe` False
    it "is not eye of black" $ do
      isEye (pass testStatusA) (coord (1,7)) `shouldBe` False
    it "is eye of white" $ do
      isEye testStatusA (coord (1,7)) `shouldBe` True
    it "is on stone" $ do
      isEye testStatusA (coord (8,9)) `shouldBe` False
    it "is not surrounded by any stone" $ do
      isEye testStatusA (coord (1,1)) `shouldBe` False
    it "is not surrounded by white stones only" $ do
      isEye testStatusA (coord (5,9)) `shouldBe` False
    it "is not surrounded by black stones only" $ do
      isEye (pass testStatusA) (coord (5,9)) `shouldBe` False
    it "has a slack" $ do
      isEye testStatusA (coord (1,5)) `shouldBe` False
    it "is eye" $ do
      isEye (pass testStatusA) (coord (9,1)) `shouldBe` True
    it "is false eye" $ do
      isEye testStatusA (coord (9,9)) `shouldBe` True

  let testStatusB = playout 0 initGame

  describe "playout finishes a game" $ do
    it "has no points can put (1)" $ do
      pointsCanPut testStatusB `shouldBe` []
    it "has no points can put (2)" $ do
      pointsCanPut (pass testStatusB) `shouldBe` []

  -- describe "count" $ do
  --   it "is (64,17)" $ do
  --     count testStatusB `shouldBe` (9+9+9+9+6+6+5+6+5, 0+0+0+0+3+3+4+3+4)
