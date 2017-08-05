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

import           Debug.Trace

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
      (res, state) <- runMock (SUT.save newUser) $ initState []
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
      (_, bst)   <- runMock (SUT.save beforeUpdate) $ initState []
      (_, actSt) <- runMock (SUT.save afterUpdate) bst

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
    it "If id don't exists then not changed." $ do
      (_, state) <- runMock (SUT.delete $ DU.UserId 4) $ initState allData

      let actUsrs = users state
      length actUsrs `shouldBe` length allData

      (DU.getUserId $ actUsrs !! 0)  `shouldBe` (DU.getUserId $ allData !! 0)
      (DU.getUserId $ actUsrs !! 1)  `shouldBe` (DU.getUserId $ allData !! 1)
      (DU.getUserId $ actUsrs !! 2)  `shouldBe` (DU.getUserId $ allData !! 2)
    it "If id exists then delete." $ do
      (_, state) <- runMock (SUT.delete $ DU.UserId 2) $ initState allData

      let actUsrs = users state
      length actUsrs `shouldBe` 2

      (DU.getUserId $ actUsrs !! 0)  `shouldBe` (DU.getUserId $ allData !! 0)
      (DU.getUserId $ actUsrs !! 1)  `shouldBe` (DU.getUserId $ allData !! 2)

specFindById :: Spec
specFindById = do
  describe "findById" $ do
    it "If id exists then return user." $ do
      (res, _) <- runMock (SUT.findBy $ DU.UserId 3) $ initState allData

      let actual  = fromJust res
      let expected = allData !! 2

      DU.getUserId actual            `shouldBe` (Just $ DU.UserId 3)
      (DN.value $ DU.getName actual) `shouldBe` (DN.value $ DU.getName expected)
      (DA.value $ DU.getAge actual)  `shouldBe` (DA.value $ DU.getAge expected)

    it "If id don't exists then return Nothing." $ do
      (res, _) <- runMock (SUT.findBy $ DU.UserId 4) $ initState allData

      isNothing res `shouldBe` True

specFindAll :: Spec
specFindAll = do
  describe "findAll" $ do
    it "Return all data." $ do
      (actual, _) <- runMock (SUT.findAll) $ initState allData

      let expected = allData
      length actual `shouldBe` length expected

      (DU.getUserId $ actual !! 0)  `shouldBe` (DU.getUserId $ expected !! 0)
      (DU.getUserId $ actual !! 1)  `shouldBe` (DU.getUserId $ expected !! 1)
      (DU.getUserId $ actual !! 2)  `shouldBe` (DU.getUserId $ expected !! 2)


newUser :: DU.User
newUser = extract $ DU.parse Nothing "test-user" 80

beforeUpdate :: DU.User
beforeUpdate = extract $ DU.parse (Just $ DU.UserId 100) "before-update" 40

afterUpdate :: DU.User
afterUpdate = extract $ DU.parse (Just $ DU.UserId 100) "after-update" 50

allData :: [DU.User]
allData = [data01, data02, data03]

data01 :: DU.User
data01 = extract $ DU.parse (Just $ DU.UserId 1) "data1" 10

data02 :: DU.User
data02 = extract $ DU.parse (Just $ DU.UserId 2) "data2" 20

data03 :: DU.User
data03 = extract $ DU.parse (Just $ DU.UserId 3) "data1" 30

extract :: Validation [a] b -> b
extract v = case v of
              Success x -> x

