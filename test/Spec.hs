import Test.Hspec
import CompressionSpec (spec)
import qualified EncodingSpec

main :: IO ()
main = hspec $ do
  spec
  EncodingSpec.spec
