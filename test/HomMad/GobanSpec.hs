module HomMad.GobanSpec where

import Test.Hspec
import HomMad.Goban
import qualified Data.Set as S

spec :: Spec
spec = do
  describe "aroundOf" $ do
    it "is 4 points" $ do
      aroundOf (coord (1,1)) `shouldBe` map coord [(0,1), (2,1), (1,0), (1,2)]
  let testStatusA = foldl putStone initGame $ map coord [
                     (7, 3) -- B
                    ,(1, 2) -- W
                    ,(8, 2) -- B
                    ,(2, 1) -- W
                    ,(8, 4) -- B
                    ,(6, 2) -- W
                    ,(9, 2) -- B
                    ,(6, 3) -- W
                    ,(9, 3) -- B
                    ,(6, 4) -- W
                    ,(9, 4) -- B
                    ,(7, 2) -- W
                    ,(9, 9) -- B
                    ,(7, 4) -- W
                    ]

  let testStatusD = foldl putStone testStatusA $ map coord [(8,9),(8,3)]

  let testStatusB = foldl putStone initGame $ map coord [
                     (4, 5) -- B
                    ,(3, 5) -- W
                    ,(5, 4) -- B
                    ,(5, 3) -- W
                    ,(6, 4) -- B
                    ,(6, 3) -- W
                    ,(6, 5) -- B
                    ,(7, 5) -- W
                    ,(5, 6) -- B
                    ,(4, 6) -- W
                    ,(6, 8) -- B
                    ,(4, 4) -- W
                    ,(5, 5) -- B
                    ]

  describe "getChain" $ do
    let chain1 = getChain testStatusA $ coord (7,3)
    it "has alone stone" $ do
      _chainCoords chain1  `shouldBe` S.fromList [coord (7,3)]
    it "has a liberty" $ do
      _chainLiberties chain1 `shouldBe` S.fromList [coord (8,3)]
    let chain2 = getChain testStatusA $ coord (8,4)
    it "has many stones" $ do
      _chainCoords chain2 `shouldBe`
        S.fromList (map coord [(8,2),(8,4),(9,2),(9,3),(9,4)])
    it "has liberties" $ do
      _chainLiberties chain2 `shouldBe`
        S.fromList (map coord [(8,1),(8,3),(8,5),(9,1),(9,5)])
    it "is connected" $ do
      _chainCoords (getChain testStatusB $ coord (5,5)) `shouldBe`
        S.fromList (map coord [(4,5),(5,4),(6,4),(6,5),(5,6),(5,5)])
    it "has liberty by killing" $ do
      getChain testStatusD (coord (8,3)) `shouldBe`
        Chain 1 (S.fromList [coord (8,3)]) (S.fromList [coord (7,3)])

  describe "canPut" $ do
    let testStatusC = pass testStatusA
    it "returns False for on a stone" $ do
      canPut testStatusC (coord (6,2)) `shouldBe` False
    it "returns False for out of a board" $ do
      canPut testStatusC (coord (9,10)) `shouldBe` False
    it "returns True for the point has some liberties" $ do
      canPut testStatusC (coord (4,4)) `shouldBe` True
    it "returns True for the point srrounded by same color stones" $ do
      canPut testStatusC (coord (1,1)) `shouldBe` True
    it "returns True if can capture" $ do
      canPut testStatusC (coord (8,3)) `shouldBe` True
    it "returns Flase for the ko point" $ do
      canPut testStatusD (coord (7,3)) `shouldBe` False
    it "returns True for the ko point when passed once" $ do
      canPut (pass $ pass testStatusD) (coord (7,3)) `shouldBe` True
