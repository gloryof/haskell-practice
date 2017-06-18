module Domain.NameSpec (spec) where

import           Test.Hspec
import           Domain.Name
import           Domain.Validate
import           Data.Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "validate" $ do
    it "if value length is 1 then return string as name" $ do
      case validate "a" of
        Success v -> v `shouldBe` "a"
    it "if value length is 20 then return string as name" $ do
      case validate (concat $ replicate 20 "a") of
        Success v -> v `shouldBe` (concat $ replicate 20 "a")
    it "if value length is Empty then return RequiredError" $ do
      result  <- return $ validate ""
      message <- return $
        case result of
          Failure x -> x
      length message  `shouldBe` 1
      message !! 0    `shouldBe` RequiredError "名前"
    it "if value length is 21 then return LengthError" $ do
      result  <- return $ validate (concat $ replicate 21 "a")
      message <- return $
        case result of
          Failure x -> x
      length message  `shouldBe` 1
      message !! 0    `shouldBe` LengthError "名前" 20
