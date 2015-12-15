module Taplike.Shared where

import           ClassyPrelude
import qualified Model
import qualified Types                  as TP 
import           Database.Persist.Types (Entity(..))
import           Database.Persist.Sql   (fromSqlKey)


userFromEntity :: Entity Model.User -> TP.User
userFromEntity userEntity = TP.User (TP.UserId $ fromSqlKey key) (TP.TwitterUserId $ Model.userTwitterUserId userVal) (TP.TwitterScreenName $ Model.userTwitterScreenName userVal) (TP.ProfileImageUrl $ Model.userProfileImageUrl userVal) 
  where userVal = entityVal userEntity
        key     = entityKey userEntity
        
