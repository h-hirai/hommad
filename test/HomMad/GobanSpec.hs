module HomMad.GobanSpec where

import Test.Hspec
import HomMad.Goban
import qualified Data.Set as S
import qualified Data.IntMap as IM

data PointSym = E | B | W

makeBoard :: [PointSym] -> Board Color
makeBoard syms = IM.fromList $ map g $ filter f $ zip [0..] syms
    where f (_, E) = False
          f _      = True
          g (idx, B) = (idx, Black)
          g (idx, W) = (idx, White)
          g _ = error "makeBoard"

spec :: Spec
spec = do
  let testStatusA = foldl putStone initGame [
                     (6, 2) -- B
                    ,(0, 1) -- W
                    ,(7, 1) -- B
                    ,(1, 0) -- W
                    ,(7, 3) -- B
                    ,(5, 1) -- W
                    ,(8, 1) -- B
                    ,(5, 2) -- W
                    ,(8, 2) -- B
                    ,(5, 3) -- W
                    ,(8, 3) -- B
                    ,(6, 1) -- W
                    ,(8, 8) -- B
                    ,(6, 3) -- W
                    ]

  let testStatusB = foldl putStone initGame [
                     (3,4) -- B
                    ,(2,4) -- W
                    ,(4,3) -- B
                    ,(4,2) -- W
                    ,(5,3) -- B
                    ,(5,2) -- W
                    ,(5,4) -- B
                    ,(6,4) -- W
                    ,(4,5) -- B
                    ,(3,5) -- W
                    ,(5,7) -- B
                    ,(3,3) -- W
                    ,(4,4) -- B
                    ]

  describe "getChain" $ do
    let chain1 = getChain testStatusA (6,2)
    it "has alone stone" $ do
      _chainCoords chain1  `shouldBe` S.fromList [(6,2)]
    it "has a liberty" $ do
      liberties (_board testStatusA) chain1 `shouldBe` S.fromList [(7,2)]
    let chain2 = getChain testStatusA (7,3)
    it "has many stones" $ do
      _chainCoords chain2
           `shouldBe` S.fromList [(7,1),(7,3),(8,1),(8,2),(8,3)]
    it "has liberties" $ do
      liberties (_board testStatusA) chain2
           `shouldBe` S.fromList [(7,0),(7,2),(7,4),(8,0),(8,4)]
    it "is connected" $ do
      _chainCoords (getChain testStatusB (4,4))
           `shouldBe` S.fromList [(3,4), (4,3), (5,3), (5,4), (4,5), (4,4)]

  describe "canPut" $ do
    let testStatusC = pass testStatusA
    it "returns False for on a stone" $ do
      canPut testStatusC (5,1) `shouldBe` False
    it "returns False for out of a board" $ do
      canPut testStatusC (8,9) `shouldBe` False
    it "returns True for the point has some liberties" $ do
      canPut testStatusC (3,3) `shouldBe` True
    it "returns True for the point srrounded by same color stones" $ do
      canPut testStatusC (0,0) `shouldBe` True
    it "returns True if can capture" $ do
      canPut testStatusC (7,2) `shouldBe` True
