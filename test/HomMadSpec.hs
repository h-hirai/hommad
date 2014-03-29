module HomMadSpec where

import Test.Hspec
import HomMad
import qualified Data.Set as S

spec :: Spec
spec = do
  let testBoard = [[E,E,E,E,E]
                  ,[E,W,W,W,E]
                  ,[E,W,B,W,E]
                  ,[E,B,E,B,E]
                  ,[E,B,B,B,E]]
  describe "getChain" $ do
    let chain1 = getChain testBoard (2,2)
    it "has alone stone" $ do
      _chainPoints chain1  `shouldBe` S.fromList [(2,2)]
    it "has a liberty" $ do
      _chainLiberties chain1 `shouldBe` S.fromList [(3,2)]
    let chain2 = getChain testBoard (3,3)
    it "has many stones" $ do
      _chainPoints chain2
           `shouldBe` S.fromList [(3,1),(3,3),(4,1),(4,2),(4,3)]
    it "has liberties" $ do
      _chainLiberties chain2
           `shouldBe` S.fromList [(3,0),(3,2),(3,4),(4,0),(4,4)]
