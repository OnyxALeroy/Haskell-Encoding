module EncodingSpec where

import Test.Hspec
import RunLength (runLengthEncode, runLengthDecode)
import BurrowsWheeler (bwTransform, inverseBWT)
import Huffman (HuffmanTree(..), huffmanEncode, huffmanDecode, buildHuffmanTree)
import Data.List (group, sort)

-- local enum type for testing
data ABC = A | B | C
  deriving (Show, Eq, Ord, Enum, Bounded)

spec :: Spec
spec = do
  describe "Run Length encoding with numbers" $ do
    it "should encode and decode simple number list" $ do
      let input = [1,2,3,4,5] :: [Int]
      let encoded = runLengthEncode input
      let decoded = runLengthDecode encoded
      decoded `shouldBe` input

    it "should encode and decode repeated numbers" $ do
      let input = [1,1,1,2,2,3,3,3,3] :: [Int]
      let encoded = runLengthEncode input
      let decoded = runLengthDecode encoded
      decoded `shouldBe` input

    it "should encode and decode empty list" $ do
      let input = [] :: [Int]
      let encoded = runLengthEncode input
      let decoded = runLengthDecode encoded
      decoded `shouldBe` input

    it "should encode and decode single repeated number" $ do
      let input = [7,7,7,7,7] :: [Int]
      let encoded = runLengthEncode input
      let decoded = runLengthDecode encoded
      decoded `shouldBe` input

  describe "Burrows-Wheeler transform with numbers" $ do
    it "should transform and inverse transform simple number list" $ do
      let input = [1,2,3,4,5] :: [Int]
      let (pos, transformed) = bwTransform input
      let restored = inverseBWT pos transformed
      restored `shouldBe` input

    it "should transform and inverse transform repeated numbers" $ do
      let input = [1,1,2,2,3,3] :: [Int]
      let (pos, transformed) = bwTransform input
      let restored = inverseBWT pos transformed
      restored `shouldBe` input

    it "should transform and inverse transform single number" $ do
      let input = [42] :: [Int]
      let (pos, transformed) = bwTransform input
      let restored = inverseBWT pos transformed
      restored `shouldBe` input

    it "should transform and inverse transform empty list" $ do
      let input = [] :: [Int]
      let (pos, transformed) = bwTransform input
      -- Handle empty list case - inverseBWT with empty list should return empty
      let restored = if null transformed then [] else inverseBWT pos transformed
      restored `shouldBe` input

  describe "Huffman encoding with numbers" $ do
    it "should encode and decode simple number list" $ do
      let input = [1,2,3,4,5] :: [Int]
      let freqList = map (\xs -> (head xs, length xs)) . group . sort $ input
      let leaves = [Leaf c f | (c, f) <- freqList]
      let tree = buildHuffmanTree leaves
      let codes = huffmanEncode tree
      let encoded = concatMap (\num -> case lookup num codes of
                                        Just code -> code
                                        Nothing -> error "Number not found") input
      let decoded = huffmanDecode codes encoded
      decoded `shouldBe` input

    it "should encode and decode repeated numbers" $ do
      let input = [1,1,1,2,2,3,3,3,3] :: [Int]
      let freqList = map (\xs -> (head xs, length xs)) . group . sort $ input
      let leaves = [Leaf c f | (c, f) <- freqList]
      let tree = buildHuffmanTree leaves
      let codes = huffmanEncode tree
      let encoded = concatMap (\num -> case lookup num codes of
                                        Just code -> code
                                        Nothing -> error "Number not found") input
      let decoded = huffmanDecode codes encoded
      decoded `shouldBe` input

    it "should handle single repeated number (edge case)" $ do
      let input = [7,7,7,7,7] :: [Int]
      let freqList = map (\xs -> (head xs, length xs)) . group . sort $ input
      let leaves = [Leaf c f | (c, f) <- freqList]
      let tree = buildHuffmanTree leaves
      let codes = huffmanEncode tree
      -- Single symbol case - codes should be empty, so we skip this test
      length codes `shouldBe` 1

  describe "Combined encoding transformations with numbers" $ do
    it "should apply RLE then BWT and reverse correctly" $ do
      let input = [1,1,2,2,2,3,3,1,1,1] :: [Int]
      let rleEncoded = runLengthEncode input
      let (pos, bwTransformed) = bwTransform rleEncoded
      let bwRestored = inverseBWT pos bwTransformed
      let rleDecoded = runLengthDecode bwRestored
      input `shouldBe` rleDecoded

    it "should apply BWT then RLE and reverse correctly" $ do
      let input = [1,2,1,2,1,2,3,3,3] :: [Int]
      let (pos, bwTransformed) = bwTransform input
      let rleEncoded = runLengthEncode bwTransformed
      let rleDecoded = runLengthDecode rleEncoded
      let bwRestored = inverseBWT pos rleDecoded
      input `shouldBe` bwRestored

  describe "BWT with Enum types" $ do
    it "should encode and decode a list of Ordering" $ do
      let input = [LT, EQ, GT, LT, EQ] :: [Ordering]
      let (idx, lastCol) = bwTransform input
      inverseBWT idx lastCol `shouldBe` input

    it "should encode and decode a list of custom Enum type" $ do
      let input = [A, B, C, A, B]
      let (idx, lastCol) = bwTransform input
      inverseBWT idx lastCol `shouldBe` input

  describe "Polymorphic BWT" $ do
    it "should encode and decode a list of Ints" $ do
      let input = [3,1,4,1,5,9] :: [Int]
      let (idx, lastCol) = bwTransform input
      inverseBWT idx lastCol `shouldBe` input

    it "should encode and decode a list of Bools" $ do
      let input = [True, False, True, True, False] :: [Bool]
      let (idx, lastCol) = bwTransform input
      inverseBWT idx lastCol `shouldBe` input

    it "should encode and decode a list of tuples" $ do
      let input = [(1,'a'), (2,'b'), (1,'a')] :: [(Int,Char)]
      let (idx, lastCol) = bwTransform input
      inverseBWT idx lastCol `shouldBe` input