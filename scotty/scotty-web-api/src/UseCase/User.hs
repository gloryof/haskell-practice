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

import qualified Domain.User as DU

import qualified Infra.Repository.User as IF


-- | Save user.
-- If user is registered then update.
-- If user is not registered then register.
save :: IF.UserRepository m => DU.User -> m (DU.UserId)
save = IF.register

-- | Delete user.
delete :: (IF.UserRepository m) => DU.UserId -> m ()
delete = IF.delete

-- | Find user by userid.
findBy :: (IF.UserRepository m) => DU.UserId -> m (Maybe DU.User)
findBy = IF.findBy

-- | Find all users.
findAll :: (IF.UserRepository m) => m ([DU.User])
findAll = IF.findAll

