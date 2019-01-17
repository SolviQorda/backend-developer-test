{-# LANGUAGE OverloadedStrings #-}

module Handler.HostUserGames where

import Import
import Model

postHostGamesR :: Handler Value
postHostGamesR = do
  subject <- getValidGoogleSubject
  _ <- runDB $ deleteWhere [HostDetailsHostId ==. subject]
  --get the user profile
  profile <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  --insert all possible games into the db
  games <- runDB $ insert $ parseHost subject (entityVal $ Prelude.head profile)
  -- redirect ShowGamesR
  -- return $ toJSON $ parseGames authId (entityVal $ Prelude.head profile)
  sendResponseStatus status201 ("HOST STATUS PUBLISHED" :: Text)

parseHost :: Text -> UserProfile -> HostDetails
parseHost subject userProfile =
      HostDetails
          (userProfileGames userProfile)
          (userProfileLongitude userProfile)
          (userProfileLatitude userProfile)
          (userProfileName userProfile)
          subject
