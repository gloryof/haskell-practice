{-|
Module       : Infra.Repository.User.
Description  : Repository user module.

This module is repository that is about user.
-}
module Infra.Repository.User (
  UserRepository(..)
) where


-- | User repository.
class UserRepository where
  -- | Save user.
  -- If user is registered then update.
  -- If user is not registered then register.
  save   :: User   -> IO (UserId)
  -- | Delete user.
  delete :: UserId -> IO ()
  -- | Find user by UserId.
  findBy :: UserId -> IO (UserId)
  
