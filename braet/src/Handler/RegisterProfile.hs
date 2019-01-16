{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}


module Handler.RegisterProfile where

import Import
import qualified Database.Persist.Class as P
import Data.Aeson
import Data.Maybe
import GHC.Generics

--make a new profile
postRegisterProfileR :: Handler Value
postRegisterProfileR = do
  (authId, user) <- requireAuthPair
  inputProfile   <- requireJsonBody :: Handler InputProfile
  _              <- runDB $ insert $ makeProfile inputProfile authId
  -- return $ toJSON $ makeProfile inputProfile authId
  sendResponseStatus status201 ("PROFILE REGISTERED" :: Text)

--update an existing profile
-- putRegisterProfileR :: Value -> Handler Value
-- putRegisterProfileR inputProfile = do
--   (authId, user) <- requireAuthPair
--   old
--   _              <- runDB $ replace authId $ makeProfile inputProfile authId
--   return $ toJSON $ makeProfile inputProfile authId

makeProfile :: InputProfile -> UserId -> UserProfile
makeProfile inputProfile userId =
  UserProfile
    { userProfilePlayerId        = userId
    , userProfileName            = name inputProfile
    , userProfileLongitude       = longitude inputProfile
    , userProfileLatitude        = latitude inputProfile
    , userProfileGames           = games inputProfile
    , userProfileAge             = age inputProfile
    , userProfileAvailableToHost = availableToHost inputProfile
    }
