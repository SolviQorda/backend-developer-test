{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}

module Handler.RequestGame where

import Import

postRequestGameR :: Handler Value
postRequestGameR = do
  subject <- getValidGoogleSubject
  host' <- requireJsonBody :: Handler Text
  _  <- runDB $ insert $ JoinGameRequest subject host'
  sendResponseStatus status201 ("Request sent" :: Text)

getRequestGameR :: Handler Value
getRequestGameR = do
  subject <- getValidGoogleSubject
  profiles <- runDB $ do
    requests <- selectList [JoinGameRequestHost ==. subject] []
    forM requests $ \(Entity _ (JoinGameRequest player _)) -> do
      profiles <- selectList [UserProfilePlayerId ==. player] []
      return $ Prelude.map entityVal profiles
  sendResponseStatus status200 $ toJSON profiles
