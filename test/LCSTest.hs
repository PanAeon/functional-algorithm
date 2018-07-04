module LCSTest where

import           LCS
import           Test.Tasty.Hspec




spec_lcs :: Spec
spec_lcs = do
  describe "LCS Plain" $ do
    it "should satisfy simple test" $ do
      let (xs,ys) = ([11, 1,12,2,13,3,16], [0,1,7,8,2,7,9,3])
      lcsPlain xs ys `shouldBe`  [1,2,3]
    it "should satisfy example test" $ do
      let (xs, ys) = ("ABCDGH", "AEDFHR")
      lcsPlain xs ys `shouldBe` "ADH"
    it "should satisfy second example" $ do
      let (xs, ys) = ("AGGTAB", "GXTXAYB")
      lcsPlain xs ys `shouldBe` "GTAB"
    it "should satisfy reverse second example" $ do
     let (xs, ys) = ("GXTXAYB", "AGGTAB")
     lcsPlain xs ys `shouldBe` "GTAB"
  describe "LCS slow" $ do
    it "should satisfy simple test" $ do
      let (xs,ys) = ([11, 1,12,2,13,3,16], [0,1,7,8,2,7,9,3])
      lcsSlow xs ys `shouldBe`  [1,2,3]
    it "should satisfy example test" $ do
      let (xs, ys) = ("ABCDGH", "AEDFHR")
      lcsSlow xs ys `shouldBe` "ADH"
    it "should satisfy second example" $ do
      let (xs, ys) = ("AGGTAB", "GXTXAYB")
      lcsSlow xs ys `shouldBe` "GTAB"
    it "should satisfy reverse second example" $ do
     let (xs, ys) = ("GXTXAYB", "AGGTAB")
     lcsSlow xs ys `shouldBe` "GTAB"
  describe "LCS cached" $ do
    it "should satisfy simple test" $ do
      let (xs,ys) = ([11, 1,12,2,13,3,16], [0,1,7,8,2,7,9,3])
      lcsCached xs ys `shouldBe`  [1,2,3]
    it "should satisfy example test" $ do
      let (xs, ys) = ("ABCDGH", "AEDFHR")
      lcsCached xs ys `shouldBe` "ADH"
    it "should satisfy second example" $ do
      let (xs, ys) = ("AGGTAB", "GXTXAYB")
      lcsCached xs ys `shouldBe` "GTAB"
    it "should satisfy reverse second example" $ do
     let (xs, ys) = ("GXTXAYB", "AGGTAB")
     lcsCached xs ys `shouldBe` "GTAB"
