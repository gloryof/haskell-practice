module Domain.AgeSpec (spec) where

import           Test.Hspec
import           Domain.Age
import           Domain.Validate
import           Data.Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "validate" $ do
    it "If value is 0 then return number as age." $ do
      case validate 0 of
        Success v -> v `shouldBe` 0
    it "If value is 200 then return number as age." $ do
      case validate 200 of
        Success v -> v `shouldBe` 200
    it "If value is 201 then return RangeError." $ do
      result  <- return $ validate 201
      message <- return $
        case result of
          Failure x -> x
      length message  `shouldBe` 1
      message !! 0    `shouldBe` RangeError "年齢" 0 200
    it "If value is -1 then return RangeError." $ do
      result  <- return $ validate (-1)
      message <- return $
        case result of
          Failure x -> x
      length message  `shouldBe` 1
      message !! 0    `shouldBe` RangeError "年齢" 0 200
