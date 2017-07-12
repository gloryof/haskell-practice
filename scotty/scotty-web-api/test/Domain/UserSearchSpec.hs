module Domain.UserSearchSpec (spec) where

import           Test.Hspec
import           Domain.UserSearch
import qualified Domain.Age  as DA
import qualified Domain.Name as DN
import qualified Domain.User as DU
import           Domain.Validate
import           Data.Validation


main :: IO ()
main = hspec spec

data ExpectedValue =
  ExpectedValue
    {
      expIds   :: [DU.UserId]
    , expName  :: Maybe DN.Name
    , expStart :: Maybe DA.Age
    , expEnd   :: Maybe DA.Age
    }

spec :: Spec
spec = do
  specParse

specParse :: Spec
specParse = do
  describe "parse" $ do
    it "If all value are nothing or empty then return empty condition" $ do
      ic <- return $
        InputCondition {
            userIds = []
          , name = Nothing
          , ageRange = (Nothing, Nothing)
        }
      ac <- return $ parseSuccess  $ parse ic
      ex <- return $ ExpectedValue [] Nothing Nothing Nothing
      assertCondition ac ex
    it "If all value are valid then return condition" $ do
      ic <- return $
        InputCondition {
            userIds = [ 100, 200, 300 ]
          , name = Just "test-name"
          , ageRange = (Just 50, Just 60)
        }
      ac <- return $ parseSuccess $ parse ic
      ex <- return $
        ExpectedValue
          {
              expIds   = [DU.UserId 100, DU.UserId 200, DU.UserId 300]
            , expName  = Just $ nameValue "test-name"
            , expStart = Just $ ageValue 50
            , expEnd   = Just $ ageValue 60
          }
      assertCondition ac ex
    it "If start of range and end of range are equal then return condition" $ do
      ic <- return $
        InputCondition {
            userIds = []
          , name = Nothing
          , ageRange = (Just 50, Just 50)
        }
      ac <- return $ parseSuccess $ parse ic
      ex <- return $
        ExpectedValue
          {
              expIds   = []
            , expName  = Nothing
            , expStart = Just $ ageValue 50
            , expEnd   = Just $ ageValue 50
          }
      assertCondition ac ex
    it "If name is invalid value are valid then return validataion" $ do
      ic <- return $
              InputCondition {
                  userIds  = []
                , name     = Just "123456789012345678901"
                , ageRange = (Nothing, Nothing)
              }
      ac <- return $ parseFail $ parse ic
      ex <- return $ [LengthError "名前" 20]
      ac `shouldBe` ex
    it "If start of ageRange is invalid value are valid then return validataion" $ do
      ic <- return $
              InputCondition {
                  userIds  = []
                , name     = Nothing
                , ageRange = (Just (-1), Nothing)
              }
      ac <- return $ parseFail $ parse ic
      ex <- return $ [RangeError "年齢" 0 200]
      ac `shouldBe` ex
    it "If end of ageRange is invalid value are valid then return validataion" $ do
      ic <- return $
              InputCondition {
                  userIds  = []
                , name     = Nothing
                , ageRange = (Nothing, Just 201)
              }
      ac <- return $ parseFail $ parse ic
      ex <- return $ [RangeError "年齢" 0 200]
      ac `shouldBe` ex
    it "If start of ageRange over end of ageRand then return validataion" $ do
      ic <- return $
              InputCondition {
                  userIds  = []
                , name     = Nothing
                , ageRange = (Just 51, Just 50)
              }
      ac <- return $ parseFail $ parse ic
      ex <- return $ [RangeMagnitudeError "年齢の開始" "年齢の終了"]
      ac `shouldBe` ex

parseSuccess :: Validation [SpecError] Condition -> Condition
parseSuccess x = case x of
                   Success v -> v

parseFail :: Validation [SpecError] Condition -> [SpecError]
parseFail x = case x of
                Failure v -> v

assertCondition :: Condition -> ExpectedValue -> Expectation
assertCondition cd ex = do
  (getUserIds cd)   `shouldBe` (expIds ex)
  (getName cd)      `shouldBe` (expName ex)
  (getStartAge cd)  `shouldBe` (expStart ex)
  (getEndAge cd)    `shouldBe` (expEnd ex)

ageValue :: Int -> DA.Age
ageValue x = case DA.parse x of
               Success v -> v

nameValue :: String -> DN.Name
nameValue x = case DN.parse x of
                Success v -> v
