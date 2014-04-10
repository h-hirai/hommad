module HomMad.AISpec where

import Test.Hspec
import HomMad.Goban
import HomMad.AI

spec :: Spec
spec = do
  let testBoard = [[E,E,W,E,W,W,E,W,E]
                  ,[E,E,W,W,W,E,W,W,W]
                  ,[E,E,B,B,E,E,E,E,E]
                  ,[E,E,B,E,B,B,E,E,E]
                  ,[E,E,E,B,E,B,E,E,E]
                  ,[E,E,E,B,B,B,E,E,E]
                  ,[B,B,B,E,E,E,E,E,E]
                  ,[B,E,B,E,E,E,E,E,W]
                  ,[E,B,E,E,E,E,E,W,E]]

  describe "isEye" $ do
    it "is simple eye" $ do
      isSimpleEye testBoard B (4,4) `shouldBe` True
    it "is not eye of white" $ do
      isSimpleEye testBoard W (4,4) `shouldBe` False
    it "not empty point is not eye" $ do
      isSimpleEye testBoard W (7,8) `shouldBe` False
    it "is not simple eye" $ do
      isSimpleEye testBoard B (3,3) `shouldBe` False
    it "is not combined eye" $ do
      isSimpleEye testBoard B (3,3) `shouldBe` False
    it "is not treated as combined eye " $ do
      isCombinedEye testBoard W (0,6) `shouldBe` False
    it "is combined eye" $ do
      isCombinedEye testBoard B (8,0) `shouldBe` True
