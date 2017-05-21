{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Infra.Repository.User (
  setup,
  save,
  selectAll,
  selectById
) where

import qualified Domain.User as DU
import qualified Infra.Repository.Sql as RP
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sqlite
import           Database.Persist.Sql()
import           GHC.Int


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserEntity
  name String
  age Int
  deriving Show
|]

setup :: IO ()
setup = do
  RP.query $ runMigration migrateAll

save :: DU.User -> IO (Int64)
save u = do
  key <- RP.query $ insert en
  return $ fromSqlKey key
  where
    nm = DU.name u
    ag = DU.age u
    en = UserEntity nm ag

selectAll :: IO [DU.User]
selectAll = do
  rs <- RP.query $ selectList [] [Asc UserEntityId]
  return $ map cnv rs

selectById :: Int64 -> IO (Maybe DU.User)
selectById i = do
  e <- RP.query $ selectFirst [ UserEntityId ==. key ] []
  return $ fmap cnv e
  where
    key = toSqlKey i

cnv :: Entity UserEntity -> DU.User
cnv e = DU.createUser ui nm ag
        where
          key = fromSqlKey $ entityKey e
          ui = fromIntegral key
          ev = entityVal e
          nm = userEntityName ev
          ag = userEntityAge ev
