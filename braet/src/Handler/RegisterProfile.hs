{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}


module Handler.RegisterProfile where

import Import
import qualified Database.Persist.Class as P
import Data.Aeson
import Data.Maybe
import GHC.Generics

--make a new profile
postRegisterProfileR :: InputProfile -> Handler Value
postRegisterProfileR inputProfile = do
  (authId, user) <- requireAuthPair
  _              <- runDB $ insert $ makeProfile inputProfile authId
  return $ toJSON $ makeProfile inputProfile authId

--update an existing profile
-- putRegisterProfileR :: Value -> Handler Value
-- putRegisterProfileR inputProfile = do
--   (authId, user) <- requireAuthPair
--   old
--   _              <- runDB $ replace authId $ makeProfile inputProfile authId
--   return $ toJSON $ makeProfile inputProfile authId

instance FromJSON InputProfile where
  parseJSON (Object v) =
--     -- withObject "InputProfile" $ \v ->
      InputProfile
    <$> v .: "name"
    <*> v .: "longitude"
    <*> v .: "latitude"
    <*> v .: "games"
    <*> v .: "age"
    <*> v .: "availableToHost"

instance ToJSON UserProfile where
  toJSON (UserProfile playerId name longitude latitude games age availableToHost) =
    object
        [ "Name" .= name
        --want to cast this to a Double for sorting
        , "Longitude" .= longitude
        , "Latitude" .= latitude
        , "My games" .= games
        , "My age" .= age
        , "Available to host" .= availableToHost
        ]

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

data InputProfile = InputProfile
  { name            :: Text
  , longitude       :: Double
  , latitude        :: Double
  , games           :: [Text]
  , age             :: Int
  , availableToHost :: Bool
  } deriving (Eq, Show, Generic)
