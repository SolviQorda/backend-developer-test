{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}

module Handler.RequestGame where

import Import

postRequestGameR :: Handler Value
postRequestGameR = do
  subject <- getValidGoogleSubject
  host' <- requireJsonBody :: Handler Text
  host'' <- runDB $ selectList [UserProfilePlayerId ==. host'] []
  if userProfileAvailableToHost $ Prelude.head $ Prelude.map entityVal host''
    then do
      _  <- runDB $ insert $ JoinGameRequest subject host'
      sendResponseStatus status201 ("Request sent" :: Text)
    else
      sendResponseStatus status400 ("User is not currently available to host." :: Text)

getRequestGameR :: Handler Value
getRequestGameR = do
  subject <- getValidGoogleSubject
  profiles <- runDB $ do
    requests <- selectList [JoinGameRequestHost ==. subject] []
    forM requests $ \(Entity _ (JoinGameRequest player _)) -> do
      profiles <- selectList [UserProfilePlayerId ==. player] []
      return $ Prelude.map entityVal profiles
  sendResponseStatus status200 $ toJSON profiles
