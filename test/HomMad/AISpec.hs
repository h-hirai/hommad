module HomMad.AISpec where

import Test.Hspec
import HomMad.Goban
import HomMad.AI

spec :: Spec
spec = do
  let testBoardA = [[E,E,W,E,W,W,E,W,E]
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
      isSimpleEye testBoardA B (4,4) `shouldBe` True
    it "is not eye of white" $ do
      isSimpleEye testBoardA W (4,4) `shouldBe` False
    it "not empty point is not eye" $ do
      isSimpleEye testBoardA W (7,8) `shouldBe` False
    it "is not simple eye" $ do
      isSimpleEye testBoardA B (3,3) `shouldBe` False
    it "is not combined eye" $ do
      isSimpleEye testBoardA B (3,3) `shouldBe` False
    it "is not treated as combined eye " $ do
      isCombinedEye testBoardA W (0,6) `shouldBe` False
    it "is combined eye" $ do
      isCombinedEye testBoardA B (8,0) `shouldBe` True

  let testBoardB = [[W,W,W,W,W,E,B,W,W]
                   ,[W,E,W,W,E,W,W,B,W]
                   ,[W,W,W,E,W,W,B,B,W]
                   ,[B,B,B,W,W,B,B,W,W]
                   ,[B,B,B,B,B,B,B,W,W]
                   ,[B,B,B,B,B,W,W,W,E]
                   ,[B,B,B,B,W,W,W,E,W]
                   ,[B,E,B,B,W,W,W,W,E]
                   ,[E,B,E,B,W,E,W,W,W]]

  describe "isSimpleEye" $ do
    it "is not SimpleEye" $ do
      isSimpleEye testBoardB W (0,5) `shouldBe` False
    it "is not CombinedEye" $ do
      isCombinedEye testBoardB W (0,5) `shouldBe` False

  describe "pointsCanPut" $ do
    it "is finish" $ do
      pointsCanPut initGame{_board=testBoardB, _turn=B} `shouldBe` []
    it "is last" $ do
      pointsCanPut initGame{_board=testBoardB, _turn=W} `shouldBe` [(0,5)]
