module UseCase.UserSpec(spec) where

import qualified UseCase.User as SUT

import           Test.Hspec
import           Infra.Repository.UserMock

import           Control.Monad.State

import           Data.Validation
import           Data.Maybe

import qualified Domain.User as DU
import qualified Domain.Age  as DA
import qualified Domain.Name as DN


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specSave
  specDelete
  specFindById
  specFindAll

specSave :: Spec
specSave = do
  describe "save" $ do
    it "New user add to repository. Return generated UserId" $ do
      (res, state) <- runMock (SUT.save newUser) initState
      res `shouldBe` DU.UserId 1

      let actUsrs = users state
      length actUsrs `shouldBe` 1

      let actMaybeUsr = recentry state
      isJust actMaybeUsr  `shouldBe` True

      let actUsr = fromJust actMaybeUsr
      DU.getUserId actUsr            `shouldBe` (Just $ DU.UserId 1)
      (DN.value $ DU.getName actUsr) `shouldBe` "test-user"
      (DA.value $ DU.getAge actUsr)  `shouldBe` 80
    it "Exisist user is update." $ do
      (res1, bst)   <- runMock (SUT.save beforeUpdate) initState
      (res2, actSt) <- runMock (SUT.save afterUpdate) bst

      let actUsrs = users actSt
      length actUsrs `shouldBe` 1

      let actMaybeUsr = recentry actSt
      isJust actMaybeUsr  `shouldBe` True

      let actUsr = fromJust actMaybeUsr
      DU.getUserId actUsr            `shouldBe` (Just $ DU.UserId 100)
      (DN.value $ DU.getName actUsr) `shouldBe` "after-update"
      (DA.value $ DU.getAge actUsr)  `shouldBe` 50

specDelete :: Spec
specDelete = do
  describe "delete" $ do
    it "test" $ do
      True `shouldBe` True

specFindById :: Spec
specFindById = do
  describe "findById" $ do
    it "test" $ do
      True `shouldBe` True

specFindAll :: Spec
specFindAll = do
  describe "findAll" $ do
    it "test" $ do
      True `shouldBe` True

newUser :: DU.User
newUser = extract $ DU.parse Nothing "test-user" 80

beforeUpdate :: DU.User
beforeUpdate = extract $ DU.parse (Just $ DU.UserId 100) "before-update" 40

afterUpdate :: DU.User
afterUpdate = extract $ DU.parse (Just $ DU.UserId 100) "after-update" 50

extract :: Validation [a] b -> b
extract v = case v of
              Success x -> x