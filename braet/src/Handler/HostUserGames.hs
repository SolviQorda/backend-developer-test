{-# LANGUAGE OverloadedStrings #-}

module Handler.HostUserGames where

import Import

postHostUserGamesR :: Handler Value
postHostUserGamesR = do
    (authId, user) <- requireAuthPair
    _        <- runDB $
                    deleteWhere
                      [HostedGameHostId ==. authId]
    --get the user profile
    profile  <- runDB $
                    selectList
                      [UserProfilePlayerId ==. authId]
                      [Asc UserProfilePlayerId]
    --insert all possible games into the db
    games    <- runDB $
                    insertMany $
                      parseGames authId (entityVal $ Prelude.head profile)
    -- redirect ShowGamesR
    return $ toJSON $ parseGames authId (entityVal $ Prelude.head profile)

instance ToJSON HostedGame where
  toJSON (HostedGame title longitude latitude host hostId) =
    object
      [ "Title"     .= title
      , "longitude" .= longitude
      , "Latitude"  .= latitude
      , "Host"      .= host
      ]

parseGames :: UserId -> UserProfile -> [HostedGame]
parseGames userId userProfile =
   Prelude.map makeGame (userProfileGames userProfile)
-- It would be preferable to get this type safety back somehow.
  where
    makeGame :: Text -> HostedGame
    makeGame game =
      HostedGame
          game
          (userProfileLongitude userProfile)
          (userProfileLatitude userProfile)
          (userProfileName userProfile)
          userId
