{-|
Module       : UseCase.user
Description  : UseCase user module.

This module is use case that is about user.
-}
module UseCase.User (
) where

import           Domain.User

-- | Register user.
-- When registering is finished then return UserId that was numbered new.
register :: User -> IO (UserId)

-- | Update user.
update :: User -> IO ()

-- | Delete user.
delete :: UserId -> IO ()

-- | Find user by UserId.
findBy :: UserId -> User

-- | Get all users.
getAll :: [User]


