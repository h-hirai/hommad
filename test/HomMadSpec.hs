module HomMadSpec where

import Test.Hspec
import HomMad
import qualified Data.Set as S

spec :: Spec
spec = do
  let testBoard = [[E,E,E,E,E,E,E,E,E]
                  ,[E,E,E,E,E,E,E,E,E]
                  ,[E,E,E,E,E,E,E,E,E]
                  ,[E,E,E,E,E,E,E,E,E]
                  ,[E,E,E,E,E,E,E,E,E]
                  ,[E,W,W,W,E,E,E,E,E]
                  ,[E,W,B,W,E,E,E,E,E]
                  ,[E,B,E,B,E,E,E,E,E]
                  ,[E,B,B,B,E,E,E,E,E]]
  describe "getChain" $ do
    let chain1 = getChain testBoard (6,2)
    it "has alone stone" $ do
      _chainPoints chain1  `shouldBe` S.fromList [(6,2)]
    it "has a liberty" $ do
      _chainLiberties chain1 `shouldBe` S.fromList [(7,2)]
    let chain2 = getChain testBoard (7,3)
    it "has many stones" $ do
      _chainPoints chain2
           `shouldBe` S.fromList [(7,1),(7,3),(8,1),(8,2),(8,3)]
    it "has liberties" $ do
      _chainLiberties chain2
           `shouldBe` S.fromList [(7,0),(7,2),(7,4),(8,0),(8,4)]
