{-|
Module       : UseCase.user
Description  : UseCase user module.

This module is use case that is about user.
-}
module UseCase.User (
  save,
  delete,
  findBy,
  findAll
) where

import           Data.Maybe

import qualified Domain.User as DU

import qualified Infra.Repository.User as IF

-- | Save user.
-- If user is registered then update.
-- If user is not registered then register.
save :: IF.UserRepository m => DU.User -> m (DU.UserId)
save u = do
  rs <- findSame u
  case rs of
    Nothing -> IF.register u
    Just x  -> IF.update u

-- | Delete user.
delete :: (IF.UserRepository m) => DU.UserId -> m ()
delete ui = do
  u <- findBy ui
  case u of
    Nothing -> return ()
    _       -> IF.delete ui

-- | Find user by userid.
findBy :: (IF.UserRepository m) => DU.UserId -> m (Maybe DU.User)
findBy = IF.findBy

-- | Find all users.
findAll :: (IF.UserRepository m) => m ([DU.User])
findAll = IF.findAll

-- | Search same user.
-- When user exists then return User.
-- Otherewise retrurn Nothing.
findSame :: IF.UserRepository m => DU.User -> m (Maybe DU.User)
findSame u = do
  case DU.getUserId u of
    Nothing -> return $ Nothing
    Just x  -> IF.findBy x
