{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}

module Handler.RequestGame where

import Import

postRequestGameR :: Handler Value
postRequestGameR = do
  authId          <- requireAuthId
  gameRequestedId <- requireJsonBody :: Handler HostDetailsRequestId
  _               <- runDB $
                      insert $
                        JoinGameRequest authId (requestId gameRequestedId :: HostDetailsId)
  sendResponseStatus status201 ("REQUEST SENT" :: Text)
