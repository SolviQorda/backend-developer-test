{-# LANGUAGE OverloadedStrings #-}

module Handler.HostUserGames where

import Import

postHostUserGamesR :: Handler Value
postHostUserGamesR = do
  (authId, user) <- requireAuthPair
  _ <- runDB $ deleteWhere [HostedGameHostId ==. authId]
  --get the user profile
  profile <- runDB $ selectList [UserProfilePlayerId == authId]
  _ <- runDB $ insertMany $ parseGames authId $ (entityVal $ Prelude.head profile)
  redirect $ ShowGamesR

parseGames :: UserId -> UserProfile -> [HostedGame]
parseGames userId userProfile =
   Prelude.map makeGame (userProfileGames userProfile)
-- It would be preferable to get this type safety back somehow.
  where
    makeGame :: Text -> HostedGame
    makeGame game =
      HostedGame
          game
          (Prelude.read $ unpack (userProfileLongitude userProfile) :: Double)
          (Prelude.read $ unpack (userProfileLatitude userProfile) :: Double)
          (userProfileName userProfile)
          userId
