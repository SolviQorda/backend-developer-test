{-# LANGUAGE OverloadedStrings #-}

module Handler.HostUserGames where

import Import
import Model

postHostGamesR :: Handler Value
postHostGamesR = do
    (authId, user) <- requireAuthPair
    _        <- runDB $
                    deleteWhere
                      [HostDetailsHostId ==. authId]
    --get the user profile
    profile  <- runDB $
                    selectList
                      [UserProfilePlayerId ==. authId]
                      [Asc UserProfilePlayerId]
    --insert all possible games into the db
    games    <- runDB $
                    insert $ parseHost authId (entityVal $ Prelude.head profile)
    -- redirect ShowGamesR
    -- return $ toJSON $ parseGames authId (entityVal $ Prelude.head profile)
    sendResponseStatus status201 ("HOST STATUS PUBLISHED" :: Text)

parseHost :: UserId -> UserProfile -> HostDetails
parseHost userId userProfile =
--    Prelude.map makeGame (userProfileGames userProfile)
-- -- It would be preferable to get this type safety back somehow.
--   where
--     makeGame :: Text -> HostDetails
--     makeGame game =
      HostDetails
          (userProfileGames userProfile)
          (userProfileLongitude userProfile)
          (userProfileLatitude userProfile)
          (userProfileName userProfile)
          userId
