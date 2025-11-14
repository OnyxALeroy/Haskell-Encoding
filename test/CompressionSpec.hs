module CompressionSpec where

import Test.Hspec
import Compression (compress, decompress)

spec :: Spec
spec = do
  describe "compress and decompress" $ do
    it "should work with simple string" $ do
      let input = "hello world"
      let compressed = compress input
      let decompressed = decompress compressed
      decompressed `shouldBe` input

    it "should work with repeated characters" $ do
      let input = "aaaaabbbbcccdde"
      let compressed = compress input
      let decompressed = decompress compressed
      decompressed `shouldBe` input

    it "should work with special characters" $ do
      let input = "hello\nworld\ttest\r\n"
      let compressed = compress input
      let decompressed = decompress compressed
      decompressed `shouldBe` input

    it "should work with numbers as characters" $ do
      let input = "1234567890"
      let compressed = compress input
      let decompressed = decompress compressed
      decompressed `shouldBe` input

    it "should work with mixed content" $ do
      let input = "The quick brown fox jumps over the lazy dog 123!"
      let compressed = compress input
      let decompressed = decompress compressed
      decompressed `shouldBe` input
