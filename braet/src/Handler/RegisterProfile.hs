{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}

module Handler.RegisterProfile where

import Import
import qualified Database.Persist.Class as P
import Data.Aeson
import Data.Maybe
import GHC.Generics
import qualified Data.Foldable as Foldable
--make a new profile
postRegisterProfileR :: Handler Value
postRegisterProfileR = do
  subject <- getValidGoogleSubject
  oldProfile  <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  inputProfile <- requireJsonBody :: Handler InputProfile
  if Foldable.length oldProfile == 0
    then do
      _  <- runDB $ insert $ makeProfile inputProfile subject
      sendResponseStatus status201 ("Profile registered" :: Text)
    else
      sendResponseStatus status400 ("Profile already registered" :: Text)

--update an existing profile
putRegisterProfileR :: Handler Value
putRegisterProfileR = do
  subject <- getValidGoogleSubject
  newProfile <- requireJsonBody :: Handler InputProfile
  oldProfile  <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  _           <- runDB $ replace (entityKey $ Prelude.head oldProfile) $ makeProfile newProfile subject
  sendResponseStatus status200 ("Profile updated" :: Text)

--get an existing profile
getRegisterProfileR :: Handler Value
getRegisterProfileR = do
  subject <- getValidGoogleSubject
  profile  <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  sendResponseStatus status200 $ toJSON $ Prelude.map entityVal profile


makeProfile :: InputProfile -> Text -> UserProfile
makeProfile inputProfile subject =
  UserProfile
    { userProfilePlayerId        = subject
    , userProfileName            = name inputProfile
    , userProfileLongitude       = longitude inputProfile
    , userProfileLatitude        = latitude inputProfile
    , userProfileGames           = games inputProfile
    , userProfileAge             = age inputProfile
    , userProfileAvailableToHost = availableToHost inputProfile
    }
