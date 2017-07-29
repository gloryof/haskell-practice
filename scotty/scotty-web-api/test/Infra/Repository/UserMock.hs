{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Infra.Repository.UserMock
(
  UserMockRepository,
  UserMockState(..),
  initState,
  runMock
) where

import           Data.Maybe

import           Domain.User

import           Infra.Repository.User

import           Control.Monad.State
import           Control.Monad.Catch

data UserMockState = UserMockState
  {
    recentry  :: Maybe User,
    users     :: [User]
  }

newtype UserMockRepository m a = UserMockRepository
  {
    app :: StateT UserMockState m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch)

deriving instance (Functor m, MonadCatch m) => MonadState UserMockState (UserMockRepository m)
deriving instance (Functor m, MonadCatch m, MonadIO m) => MonadIO (UserMockRepository m)

instance (Functor m, MonadCatch m) => UserRepository (UserMockRepository m) where
  register u = do
    modify $ addUser u
    st <- get
    return $ fromJust $ getUserId $ fromJust $ recentry st

initState :: UserMockState
initState = UserMockState
  {
    recentry  = Nothing,
    users     = []
  }

addUser :: User -> UserMockState -> UserMockState
addUser u um =
  um { recentry = Just nus, users = us ++ [nus] }
  where
    us  = users um
    nid = uid us
    nus = create nid (getName u) (getAge u)

uid :: [User] -> UserId
uid us = UserId { value  = 1 + (maxIntUid us) }

maxIntUid :: [User] -> Int
maxIntUid us = case ids of
                 [] -> 0
                 _  -> maximum ids
               where
                 ids = [intUid uid | uid <- us ]

intUid :: User -> Int
intUid u = case getUserId u of
             Just x  -> value x
             Nothing -> 0

runMock :: UserMockRepository m a -> UserMockState -> m (a, UserMockState)
runMock (UserMockRepository rp) = runStateT rp
