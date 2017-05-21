module Infra.Repository.Sql (
  query
) where

import Database.Persist.Sqlite
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger

query :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
query q = runNoLoggingT $ runResourceT $ withSqliteConn "dev.sqlite3" $ runSqlConn q

