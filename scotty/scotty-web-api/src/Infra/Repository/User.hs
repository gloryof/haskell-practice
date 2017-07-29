{-|
Module       : Infra.Repository.User.
Description  : Repository user module.

This module is repository that is about user.
-}
module Infra.Repository.User (
  UserRepository(..)
) where


import           Domain.User
import           Domain.UserSearch

-- | User repository.
class Monad m => UserRepository m where
  -- | Register user.
  register :: User   -> m (UserId)
  -- | Update user
  update   :: User   -> m (UserId)
  -- | Delete user.
  delete   :: UserId -> m ()
  -- | Find user by UserId.
  findBy   :: UserId -> m (Maybe User)
  -- | Find all users.
  findAll  :: m ([User])
