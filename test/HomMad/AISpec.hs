module HomMad.AISpec where

import Test.Hspec
import HomMad.Goban
import HomMad.AI
import qualified Data.IntMap as IM

data PointSym = E | B | W

makeBoard :: [PointSym] -> Board Color
makeBoard = IM.fromList . map g . filter f . zip [0..]
    where f (_, E) = False
          f _      = True
          g (idx, B) = (idx, Black)
          g (idx, W) = (idx, White)
          g _ = error "makeBoard"

spec :: Spec
spec = do
  let testBoardA = makeBoard [E,E,W,E,W,W,E,W,E
                             ,E,E,W,W,W,E,W,W,W
                             ,E,E,B,B,E,E,E,E,E
                             ,E,E,B,E,B,B,E,E,E
                             ,E,E,E,B,E,B,E,E,E
                             ,E,E,E,B,B,B,E,E,E
                             ,B,B,B,E,E,E,E,E,E
                             ,B,E,B,E,E,E,E,E,W
                             ,E,B,E,E,E,E,E,W,E]

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

  let testBoardB = makeBoard [W,W,W,W,W,E,B,W,W
                             ,W,E,W,W,E,W,W,B,W
                             ,W,W,W,E,W,W,B,B,W
                             ,B,B,B,W,W,B,B,W,W
                             ,B,B,B,B,B,B,B,W,W
                             ,B,B,B,B,B,W,W,W,E
                             ,B,B,B,B,W,W,W,E,W
                             ,B,E,B,B,W,W,W,W,E
                             ,E,B,E,B,W,E,W,W,W]

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
