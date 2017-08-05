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

import           Debug.Trace

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
  update u = do
    modify $ updateUser u
    st <- get
    return $ fromJust $ getUserId $  u
  delete uid = do
    st <- get
    case findById uid (users st) of
      Nothing -> error "Not exisits"
      _       -> modify $ deleteUser uid
  findBy uid = do
    st <- get
    return $ findById uid $ users st
  findAll = do
    st <- get
    return $ users st

initState :: [User] -> UserMockState
initState us = UserMockState
  {
    recentry  = case us of
                  [] -> Nothing
                  _  -> Just $ last us,
    users     = us
  }

updateUser :: User -> UserMockState -> UserMockState
updateUser u um =
  um { recentry = Just u, users = hus ++ [u] ++ tus  }
  where
    us  = users um
    tui = getUserId u
    bus = case tui of
            Nothing -> (us, [])
            Just x  -> break (matchUser x) us
    hus = fst bus
    tus = drop 1 $ snd bus

addUser :: User -> UserMockState -> UserMockState
addUser u um =
  um { recentry = Just nus, users = us ++ [nus] }
  where
    us  = users um
    nid = case getUserId u of
            Nothing -> uid us
            Just x  -> x
    nus = create nid (getName u) (getAge u)

matchUser :: UserId -> User -> Bool
matchUser ui u = (getUserId u) == (Just ui)

uid :: [User] -> UserId
uid us = UserId { value  = 1 + (maxIntUid us) }

findById :: UserId -> [User] -> Maybe User
findById ui us =
  case fus of
    []  -> Nothing
    xs  -> Just $ head xs
  where
    fus = filter (matchUser ui) us

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

deleteUser :: UserId -> UserMockState -> UserMockState
deleteUser ui um =
  um { recentry = rc, users = us} 
  where
    us = filter (\x -> getUserId x /= Just ui) $ users um
    rc = do
      crc <- recentry um
      rcid <- getUserId crc
      if rcid == ui
        then Nothing
        else Just crc
