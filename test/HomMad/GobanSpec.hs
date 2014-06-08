module HomMad.GobanSpec where

import Test.Hspec
import HomMad.Goban
import qualified Data.Set as S

spec :: Spec
spec = do
  let testStatusA = foldl putStone initGame $ map (uncurry Coord) [
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

  let testStatusD = foldl putStone testStatusA $ map (uncurry Coord) [
                     (7,8)
                    ,(7,2)]

  let testStatusB = foldl putStone initGame $ map (uncurry Coord) [
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
    let chain1 = getChain testStatusA (Coord 6 2)
    it "has alone stone" $ do
      _chainCoords chain1  `shouldBe` S.fromList [Coord 6 2]
    it "has a liberty" $ do
      _chainLiberties chain1 `shouldBe` S.fromList [Coord 7 2]
    let chain2 = getChain testStatusA (Coord 7 3)
    it "has many stones" $ do
      _chainCoords chain2
           `shouldBe` S.fromList (map (uncurry Coord) [(7,1),(7,3),(8,1),(8,2),(8,3)])
    it "has liberties" $ do
      _chainLiberties chain2
           `shouldBe` S.fromList (map (uncurry Coord) [(7,0),(7,2),(7,4),(8,0),(8,4)])
    it "is connected" $ do
      _chainCoords (getChain testStatusB (Coord 4 4))
           `shouldBe` S.fromList (map (uncurry Coord) [(3,4), (4,3), (5,3), (5,4), (4,5), (4,4)])
    it "has liberty by killing" $ do
      getChain testStatusD (Coord 7 2)
           `shouldBe` Chain 1 (S.fromList [Coord 7 2]) (S.fromList [Coord 6 2])

  describe "canPut" $ do
    let testStatusC = pass testStatusA
    it "returns False for on a stone" $ do
      canPut testStatusC (Coord 5 1) `shouldBe` False
    it "returns False for out of a board" $ do
      canPut testStatusC (Coord 8 9) `shouldBe` False
    it "returns True for the point has some liberties" $ do
      canPut testStatusC (Coord 3 3) `shouldBe` True
    it "returns True for the point srrounded by same color stones" $ do
      canPut testStatusC (Coord 0 0) `shouldBe` True
    it "returns True if can capture" $ do
      canPut testStatusC (Coord 7 2) `shouldBe` True
    it "returns Flase for the ko point" $ do
      canPut testStatusD (Coord 6 2) `shouldBe` False
    it "returns True for the ko point when passed once" $ do
      canPut (pass $ pass testStatusD) (Coord 6 2) `shouldBe` True
