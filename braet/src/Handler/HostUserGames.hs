{-# LANGUAGE OverloadedStrings #-}

module Handler.HostUserGames where

import Import
import Model

putHostGamesR :: Handler Value
putHostGamesR = do
  subject <- getValidGoogleSubject
  availableToHost' <- requireJsonBody :: Handler Bool
  profile <- runDB $ Prelude.head <$> selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  let profileValue = entityVal profile
  _ <- runDB $ replace (entityKey profile) $ profileValue { userProfileAvailableToHost = availableToHost' }
  sendResponseStatus status200 ("Profile updated" :: Text)

getHostGamesR :: Handler Value
getHostGamesR = do
  subject <- getValidGoogleSubject
  profile <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  sendResponseStatus status200 $ toJSON $ userProfileAvailableToHost $ Prelude.head $ Prelude.map entityVal profile
