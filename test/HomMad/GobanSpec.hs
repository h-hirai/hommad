module HomMad.GobanSpec where

import Test.Hspec
import HomMad.Goban
import qualified Data.Set as S

e, b, w :: Point Color
e = Empty
b = Point Black
w = Point White

spec :: Spec
spec = do
  let testBoard = [[e,w,e,e,e,e,e,e,e]
                  ,[w,e,e,e,e,e,e,e,e]
                  ,[e,e,e,e,e,e,e,e,e]
                  ,[e,e,e,e,e,e,e,e,e]
                  ,[e,e,e,e,e,e,e,e,e]
                  ,[e,w,w,w,e,e,e,e,e]
                  ,[e,w,b,w,e,e,e,e,e]
                  ,[e,b,e,b,e,e,e,e,e]
                  ,[e,b,b,b,e,e,e,e,e]]

  describe "getChain" $ do
    let chain1 = getChain testBoard (6,2)
    it "has alone stone" $ do
      _chainCoords chain1  `shouldBe` S.fromList [(6,2)]
    it "has a liberty" $ do
      _chainLiberties chain1 `shouldBe` S.fromList [(7,2)]
    let chain2 = getChain testBoard (7,3)
    it "has many stones" $ do
      _chainCoords chain2
           `shouldBe` S.fromList [(7,1),(7,3),(8,1),(8,2),(8,3)]
    it "has liberties" $ do
      _chainLiberties chain2
           `shouldBe` S.fromList [(7,0),(7,2),(7,4),(8,0),(8,4)]
    it "has opponents" $ do
      _chainOpponents chain2 `shouldBe` S.fromList [(6,1),(6,3)]

  describe "canPut" $ do
    let testStatusB = GameStatus testBoard White 0 0 Nothing
    it "returns False for on a stone" $ do
      canPut testStatusB (5,1) `shouldBe` False
    it "returns False for out of a board" $ do
      canPut testStatusB (8,9) `shouldBe` False
    it "returns True for the point has some liberties" $ do
      canPut testStatusB (3,3) `shouldBe` True
    it "returns True for the point srrounded by same color stones" $ do
      canPut testStatusB (0,0) `shouldBe` True
    it "returns True if can capture" $ do
      canPut testStatusB (7,2) `shouldBe` True
