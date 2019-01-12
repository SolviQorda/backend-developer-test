{-# LANGUAGE OverloadedStrings #-}

module Handler.RegisterProfile where

import Import
import qualified Database.Persist.Class as P

-- getRegisterProfileR :: UserId -> UserProfile -> Handler Value
-- getRegisterProfileR userId userProfile = error "Not yet implemented: getRegisterProfileR"

postRegisterProfileR :: UserId -> UserProfile -> Handler Value
postRegisterProfileR userId userProfile = do
  -- _ <- runDB $ P.insert userProfile
  return $ toJSON userProfile

-- jsonProfile :: UserProfile -> Value
-- jsonProfile profile =
--   object
--     [
--       ( "Name": (userProfileName profile)
--       --want to cast this to a Double for sorting
--       , "My Location" .= (userProfileLocation profile :: [Text])
--       , "Games I Play" .= (userProfileGames profile :: [Text])
--       , "My age": userProfileAge profile
--       , "Available to host": userProfileAvailableToHost profile
--       )
--     ]

instance ToJSON UserProfile where
  toJSON (UserProfile uid name location games age availableToHost q) =
    object
      [ "Name" .= name
        --want to cast this to a Double for sorting
        , "My Location" .= (location :: [Text])
        , "Games I Play" .= (games :: [Text])
        , "My age" .= age
        , "Available to host" .= availableToHost
      ]
