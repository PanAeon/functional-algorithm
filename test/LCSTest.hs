module LCSTest where

-- import qualified Hedgehog         as H
-- import qualified Hedgehog.Gen     as Gen
-- import qualified Hedgehog.Range   as Range
import           LCS
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck




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


-- hprop_reverse :: H.Property
-- hprop_reverse = H.property $ do
--   xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--   reverse (reverse xs) H.=== xs

-- prop_lcsCachedBehavesLikeLcsSlow :: Int -> Int -> Bool
prop_lcsCachedBehavesLikeLcsSlow = forAll (twoSizedLists) $ \z -> case z of
     (xs, ys) -> length (lcsCached xs ys) == length (lcsSlow xs ys)

prop_lcsPlainBehavesLikeLcsSlow = forAll (twoSizedLists) $ \z -> case z of
     (xs, ys) -> length (lcsPlain xs ys) == length (lcsSlow xs ys)

sizedList = resize 6 (listOf (choose (0::Int, 10)))
twoSizedLists = do
                      xs <- sizedList
                      ys <- sizedList
                      pure (xs,ys)
