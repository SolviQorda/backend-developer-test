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
  redirect $ RegisterProfileR authId testProfile

instance ToJSON User where
  toJSON (User email) =
    object
      [ "Email" .= email ]


testProfile :: UserProfile
testProfile =
  UserProfile
    { uid = "13"
    , name = "Solvi"
    , location = ["53.433146", "-2.268468"]
    , games = ["SettlersOfCatan", "Risk"]
    , age = "28"
    , availableToHost = "True"
    , q = [""]
    }

-- testID :: UserID
-- testID = 13
