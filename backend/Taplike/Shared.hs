module Taplike.Shared where

import           ClassyPrelude
import qualified Model
import qualified Types                  as TP 
import           Database.Persist.Types (Entity(..))
import           Database.Persist.Sql   (fromSqlKey)
import qualified Taplike.Schema as Schema

userFromEntity :: Entity Model.User -> TP.Presence -> TP.User
userFromEntity userEntity = TP.User (TP.UserId $ fromSqlKey key) (TP.TwitterUserId $ Model.userTwitterUserId userVal) 
                                     (TP.TwitterScreenName $ Model.userTwitterScreenName userVal) 
                                     (TP.ProfileImageUrl $ Model.userProfileImageUrl userVal)
  where userVal = entityVal userEntity
        key     = entityKey userEntity
        
messageFromEntity :: Model.Message -> TP.ChannelSlug -> [Model.MessageLike] -> TP.Message
messageFromEntity msg chanSlug likes = TP.Message (TP.UserId $ fromSqlKey (Model.messageUser msg)) 
                                                  (TP.MessageText $ Model.messageText msg) (Model.messageTimestamp msg) Nothing 
                                                  (TP.unChannelSlug chanSlug) (Schema._messageUuid $ Model.messageUuid msg) 
                                                  (likeFromEntity <$> likes)

likeFromEntity :: Model.MessageLike -> TP.MessageLike 
likeFromEntity like = TP.MessageLike (TP.MessageId $ Schema._messageUuid $ Model.messageLikeMessage like) 
                                     (TP.UserId $ fromSqlKey (Model.messageLikeUser like)) 
                                     (TP.ChannelSlug $ Schema.unSlug $ Model.messageLikeChannel like) 
                                     (Model.messageLikeTimestamp like)
        
