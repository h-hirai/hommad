module HomMad.AISpec where

import Test.Hspec
import HomMad.Goban
import HomMad.AI

e, b, w :: Point Color
e = Empty
b = Point Black
w = Point White

spec :: Spec
spec = do
  let testBoardA = [[e,e,w,e,w,w,e,w,e]
                   ,[e,e,w,w,w,e,w,w,w]
                   ,[e,e,b,b,e,e,e,e,e]
                   ,[e,e,b,e,b,b,e,e,e]
                   ,[e,e,e,b,e,b,e,e,e]
                   ,[e,e,e,b,b,b,e,e,e]
                   ,[b,b,b,e,e,e,e,e,e]
                   ,[b,e,b,e,e,e,e,e,w]
                   ,[e,b,e,e,e,e,e,w,e]]

  describe "isEye" $ do
    it "is simple eye" $ do
      isSimpleEye testBoardA Black (4,4) `shouldBe` True
    it "is not eye of white" $ do
      isSimpleEye testBoardA White (4,4) `shouldBe` False
    it "not empty point is not eye" $ do
      isSimpleEye testBoardA White (7,8) `shouldBe` False
    it "is not simple eye" $ do
      isSimpleEye testBoardA Black (3,3) `shouldBe` False
    it "is not combined eye" $ do
      isSimpleEye testBoardA Black (3,3) `shouldBe` False
    it "is not treated as combined eye " $ do
      isCombinedEye testBoardA White (0,6) `shouldBe` False
    it "is combined eye" $ do
      isCombinedEye testBoardA Black (8,0) `shouldBe` True

  let testBoardB = [[w,w,w,w,w,e,b,w,w]
                   ,[w,e,w,w,e,w,w,b,w]
                   ,[w,w,w,e,w,w,b,b,w]
                   ,[b,b,b,w,w,b,b,w,w]
                   ,[b,b,b,b,b,b,b,w,w]
                   ,[b,b,b,b,b,w,w,w,e]
                   ,[b,b,b,b,w,w,w,e,w]
                   ,[b,e,b,b,w,w,w,w,e]
                   ,[e,b,e,b,w,e,w,w,w]]

  describe "isSimpleEye" $ do
    it "is not SimpleEye" $ do
      isSimpleEye testBoardB White (0,5) `shouldBe` False
    it "is not CombinedEye" $ do
      isCombinedEye testBoardB White (0,5) `shouldBe` False

  describe "pointsCanPut" $ do
    it "is finish" $ do
      pointsCanPut initGame{_board=testBoardB, _turn=Black} `shouldBe` []
    it "is last 1" $ do
      pointsCanPut initGame{_board=testBoardB, _turn=White} `shouldBe` [(0,5)]

  let testBoardC =
          _board $ putStone initGame{_board=testBoardB, _turn=White} (0,5)

  describe "count" $ do
    it "is (32,49)" $ do
      count testBoardC `shouldBe` (4+4+4+5+7+5+2+1+0, 5+5+5+4+2+4+7+8+9)
