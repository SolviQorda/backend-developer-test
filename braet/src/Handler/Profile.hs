{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Profile where

import Import
import qualified Data.Text as T

getProfileR :: Handler Value
getProfileR = do
  (authId, user) <- requireAuthPair
  -- return $ toJSON user
  redirect $ RegisterProfileR

instance ToJSON User where
  toJSON (User email) =
    object
      [ "Email" .= email ]

-- testProfile :: UserProfile
-- testProfile =
--   UserProfile
--     { userProfilePlayerId = 13
--     , userProfileName = "Solvi"
--     , userProfileLongitude = 53.433146
--     , userProfileLatitude = -2.268468
--     , userProfileGames = ["SettlersOfCatan", "Risk"]
--     , userProfileAge = 28
--     , userProfileAvailableToHost = True
--     }
