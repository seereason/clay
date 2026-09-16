{-#LANGUAGE OverloadedStrings#-}

module Clay.SizeSpec where

import Clay.Size
import Clay.Property
import Clay.Common
import Clay.Color
import Clay.Geometry
import Clay.Render
import Clay.Stylesheet

import Test.Hspec
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.List (sort)

import Prelude hiding (rem, min, max)

sizeRepr :: Size a -> Text
sizeRepr = plain . unValue . value

hasAllPrefixes :: Val a => a -> Bool
hasAllPrefixes a = checkPrefixed ((unValue . value) a) browsers
  where checkPrefixed (Prefixed pa) (Prefixed pb) = sort (fmap fst pa) == sort (fmap fst pb)
        checkPrefixed _ _ = False

compactRender :: Css -> Text
compactRender css = toStrict $ renderWith compact [] css

spec :: Spec
spec = do
  describe "render results" $ do
    it "marginLeft auto" $
      (compactRender $ marginLeft auto) `shouldBe` "{margin-left:auto}"
    it "marginLeft normal" $
      (compactRender $ marginLeft normal) `shouldBe` "{margin-left:normal}"
    it "marginLeft inherit" $
      (compactRender $ marginLeft inherit) `shouldBe` "{margin-left:inherit}"
    it "marginLeft none" $
      (compactRender $ marginLeft none) `shouldBe` "{margin-left:none}"
  describe "simple sizes" $ do
    it "returns 1px for (px 1)" $
      sizeRepr (px 1) `shouldBe` "1px"
    it "return 50% for (pct 50)" $
      sizeRepr (pct 50) `shouldBe` "50%"
    it "returns 0.6rem for (rem 0.6)" $
      sizeRepr (rem 0.6) `shouldBe` "0.6rem"
  describe "calc addition" $ do
    it "returns proper calc for simple sum" $
      sizeRepr (em 2 @+@ px 1) `shouldBe` "calc(2em + 1px)"
    it "returns calc for nested sum" $
      sizeRepr (em 2 @+@ pt 1 @+@ px 3) `shouldBe` "calc((2em + 1pt) + 3px)"
    it "returns prefixed calc for simple sum" $
      (em 2 @+@ pt 2) `shouldSatisfy` hasAllPrefixes
    it "return calc for sum of different types" $
      sizeRepr (em 2 @+@ pct 10) `shouldBe` "calc(2em + 10%)"
    it "returns calc for simple difference" $
      sizeRepr (em 2 @-@ px 5) `shouldBe` "calc(2em - 5px)"
    it "returns calc for combination of sum and difference" $
      sizeRepr (em 2 @-@ px 5 @+@ pt 3) `shouldBe` "calc((2em - 5px) + 3pt)"
    it "returns calc for simple multiplication" $
      sizeRepr (3 *@ em 2) `shouldBe` "calc(3 * 2em)"
    it "returns calc for reversed multiplication" $
      sizeRepr (em 2 @* 3) `shouldBe` "calc(3 * 2em)"
    it "returns calc for multiplication with sum" $
      sizeRepr (em 2 @* 3 @+@ px 3) `shouldBe` "calc((3 * 2em) + 3px)"
    it "returns calc for multiplication with difference" $
      sizeRepr (vmax 5 @-@ em 2 @* 6) `shouldBe` "calc(5vmax - (6 * 2em))"
    it "behaves correctly with negatives" $
      sizeRepr (vmax (-5) @-@ em (-2) @* (-6)) `shouldBe` "calc(-5vmax - (-6 * -2em))"
    it "return calc for simple division" $
      sizeRepr (em 2 @/ 3) `shouldBe` "calc(2em / 3)"
    it "returns calc for division with sum" $
      sizeRepr (em 2 @/ 3 @+@ px 7) `shouldBe` "calc((2em / 3) + 7px)"
    it "returns correct calc for complicated expression" $
      sizeRepr (em 2 @+@ (px 3 @-@ pt 2 @/ 3) @* 4 @* 3 @/ 4 @+@ 2 *@ (vmax 3 @* 5 @+@ vmin 4))
        `shouldBe` "calc((2em + ((3 * (4 * (3px - (2pt / 3)))) / 4)) + (2 * ((5 * 3vmax) + 4vmin)))"
    it "returns original value if other is used" $
      other (value aqua) `shouldBe` (value aqua)
  describe "math functions" $ do
    it "returns min for two sizes" $
      sizeRepr (min (pct 100) (em 40)) `shouldBe` "min(100%, 40em)"
    it "returns max for two sizes" $
      sizeRepr (max (px 320) (pct 50)) `shouldBe` "max(320px, 50%)"
    it "returns clamp for three sizes" $
      sizeRepr (clamp (em 1) (vw 2.5) (em 2)) `shouldBe` "clamp(1em, 2.5vw, 2em)"
    it "combines sizes of different types" $
      sizeRepr (min (em 2) (pct 10)) `shouldBe` "min(2em, 10%)"
    it "behaves correctly with negatives" $
      sizeRepr (max (em (-2)) (px (-5))) `shouldBe` "max(-2em, -5px)"
    -- Arithmetic is allowed directly inside a math function, so the
    -- argument is a parenthesized expression rather than a nested calc.
    it "takes an arithmetic expression as an argument" $
      sizeRepr (min (pct 100 @-@ em 2) (px 960)) `shouldBe` "min((100% - 2em), 960px)"
    it "takes a math function as an argument" $
      sizeRepr (min (max (px 320) (pct 50)) (em 40))
        `shouldBe` "min(max(320px, 50%), 40em)"
    it "nests inside calc" $
      sizeRepr (min (pct 100) (em 40) @+@ px 8)
        `shouldBe` "calc(min(100%, 40em) + 8px)"
    it "nests inside clamp" $
      sizeRepr (clamp (em 1) (min (vw 5) (em 3)) (em 2))
        `shouldBe` "clamp(1em, min(5vw, 3em), 2em)"
    -- Unlike calc, these were never vendor prefixed.
    it "is not prefixed" $
      min (pct 100) (em 40) `shouldNotSatisfy` hasAllPrefixes
    it "is not prefixed by clamp" $
      clamp (em 1) (vw 2.5) (em 2) `shouldNotSatisfy` hasAllPrefixes
    it "is still prefixed when wrapped in calc" $
      (min (pct 100) (em 40) @+@ px 8) `shouldSatisfy` hasAllPrefixes
    it "renders as a property value" $
      compactRender (width (min (pct 100) (em 40)))
        `shouldBe` "{width:min(100%, 40em)}"
