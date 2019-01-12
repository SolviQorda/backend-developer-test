{-# LANGUAGE OverloadedStrings #-}

module Handler.RegisterProfile where

import Import
import qualified Database.Persist.Class as P

getRegisterProfileR :: UserId -> UserProfile -> Handler Value
getRegisterProfileR userId userProfile = return $ toJSON userProfile

postRegisterProfileR :: UserId -> UserProfile -> Handler Value
postRegisterProfileR userId userProfile = do
  -- _ <- runDB $ P.insert userProfile
  redirect $ HostUserGamesR userProfile
  return $ toJSON userProfile

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
