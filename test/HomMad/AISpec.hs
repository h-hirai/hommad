module HomMad.AISpec where

import Test.Hspec
import HomMad.Goban
import HomMad.AI

spec :: Spec
spec = do
  let testStatusA = foldl putStone initGame $ map (uncurry Coord) [
                     (2, 2) -- B
                    ,(0, 2) -- W
                    ,(2, 3) -- B
                    ,(0, 4) -- W
                    ,(3, 2) -- B
                    ,(0, 5) -- W
                    ,(3, 3) -- B
                    ,(0, 7) -- W
                    ,(3, 4) -- B
                    ,(1, 2) -- W
                    ,(4, 3) -- B
                    ,(1, 3) -- W
                    ,(4, 5) -- B
                    ,(1, 4) -- W
                    ,(5, 3) -- B
                    ,(1, 6) -- W
                    ,(5, 4) -- B
                    ,(1, 7) -- W
                    ,(5, 5) -- B
                    ,(1, 8) -- W
                    ,(6, 0) -- B
                    ,(7, 8) -- W
                    ,(6, 1) -- B
                    ,(8, 7) -- W
                    ,(6, 2) -- B
                    ,(3, 8) -- W
                    ,(7, 0) -- B
                    ,(4, 8) -- W
                    ,(7, 2) -- B
                    ,(5, 8) -- W
                    ,(8, 1) -- B
                    ]

  describe "isEye" $ do
    it "is simple eye" $ do
      isSimpleEye testStatusA Black (Coord 4 4) `shouldBe` True
    it "is not eye of white" $ do
      isSimpleEye testStatusA White (Coord 4 4) `shouldBe` False
    it "not empty point is not eye" $ do
      isSimpleEye testStatusA White (Coord 7 8) `shouldBe` False
    it "is not simple eye" $ do
      isSimpleEye testStatusA Black (Coord 3 3) `shouldBe` False
    it "is not combined eye" $ do
      isSimpleEye testStatusA Black (Coord 3 3) `shouldBe` False
    it "is not treated as combined eye " $ do
      isCombinedEye testStatusA White (Coord 0 6) `shouldBe` False
    it "is combined eye" $ do
      isCombinedEye testStatusA Black (Coord 8 0) `shouldBe` True

  let testStatusB = playout 0 initGame

  describe "playout finishes a game" $ do
    it "has no points can put (1)" $ do
      pointsCanPut testStatusB `shouldBe` []
    it "has no points can put (2)" $ do
      pointsCanPut (pass testStatusB) `shouldBe` []

  describe "count" $ do
    it "is (64,17)" $ do
      count testStatusB `shouldBe` (9+9+9+9+6+6+5+6+5, 0+0+0+0+3+3+4+3+4)
