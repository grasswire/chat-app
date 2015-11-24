{-# OPTIONS_GHC -fno-warn-orphans #-}

module Taplike.Instances where

import ClassyPrelude
import Database.Persist.Sql
import Yesod.Core.Dispatch (PathPiece)
import qualified Taplike.ChatRoomSlug as T

instance PersistField T.ChatRoomSlug where
    toPersistValue (T.ChatRoomSlug slug) = toPersistValue slug
    fromPersistValue (PersistText txt) = Right $ T.ChatRoomSlug txt
    fromPersistValue x = Left $ "Not a PersistText " ++ pack (show x)

instance PersistFieldSql T.ChatRoomSlug where
    sqlType _ =  SqlString
