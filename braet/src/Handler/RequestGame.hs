{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}

module Handler.RequestGame where

import Import

postRequestGameR :: Handler Value
postRequestGameR = do
  subject <- getValidGoogleSubject
  gameRequestedId <- requireJsonBody :: Handler HostDetailsRequestId
  _  <- runDB $ insert $ JoinGameRequest subject (requestId gameRequestedId :: HostDetailsId)
  sendResponseStatus status201 ("REQUEST SENT" :: Text)
