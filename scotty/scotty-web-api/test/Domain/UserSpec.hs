module Domain.UserSpec(spec) where

import           Test.Hspec

import qualified Domain.Age as DA
import qualified Domain.Name as DN
import           Domain.User
import           Domain.Validate
import           Data.Validation
import           Data.Maybe


main :: IO ()
main = hspec spec

numberedIdValue = 1001
numberedId = UserId numberedIdValue

validName = "test-name"

validAge = 100
inValidAge = -1

regUser = case (parse (Just numberedId) validName validAge) of
            Success x -> x
notRegUser = case (parse Nothing validName validAge) of
               Success x -> x

spec :: Spec
spec = do
  describe "parse" $ do
    it "If name and age are valid then return User" $ do
      r <- return $
           case (parse (Just numberedId) validName validAge) of
             Success x -> x
      a <- return $ DA.value $ getAge r
      n <- return $ DN.value $ getName r
      i <- return $ value    $ fromJust $ getUserId r
      a `shouldBe` validAge
      n `shouldBe` validName
      i `shouldBe` numberedIdValue
    it "If name or age are invalid then return SpecError" $ do
      r <- return $
           case (parse (Just numberedId) validName inValidAge) of
             Failure x -> x
      length r `shouldBe` 1
  describe "isRegistered" $ do
    it "If id is Just then return True." $ do
      r <- return $ isRegistered regUser
      r `shouldBe` True
    it "If id is Nothing then return False." $ do
      r <- return $ isRegistered notRegUser
      r `shouldBe` False
