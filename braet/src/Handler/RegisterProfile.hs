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
  sendResponseStatus status201 ("PROFILE REGISTERED" :: Text)

--update an existing profile
putRegisterProfileR :: Handler Value
putRegisterProfileR = do
  (authId, user) <- requireAuthPair
  newProfile     <- requireJsonBody :: Handler InputProfile
  oldProfile     <- runDB $ selectList [UserProfilePlayerId ==. authId] [Asc UserProfilePlayerId]
  _              <- runDB $ replace (entityKey $ Prelude.head oldProfile) $ makeProfile newProfile authId
  sendResponseStatus status200 ("PROFILE UPDATED" :: Text)

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
