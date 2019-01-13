{-# LANGUAGE OverloadedStrings #-}

module Handler.HostUserGames where

import Import

getHostUserGamesR :: UserId -> UserProfile -> Handler Value
getHostUserGamesR userId userProfile = do
  _ <- runDB $ deleteWhere [HostedGameHostId ==. userId]
  _ <- runDB $ insertMany $ parseGames userId $ userProfile
  redirect $ ShowGamesR userId

-- postHostUserGamesR :: UserId -> UserProfile -> Handler Html
-- postHostUserGamesR userId userProfile = do
--   _ <- runDB $ insertMany $ parseGames userProfile
--   redirect $ ShowGamesR userId

parseGames :: UserId -> UserProfile -> [HostedGame]
parseGames userId userProfile =
   Prelude.map makeGame (games userProfile)
-- It would be preferable to get this type safety back somehow.
  where
    makeGame :: Text -> HostedGame
    makeGame game =
      HostedGame
          game
          (parseLocation $ location userProfile)
          (name userProfile)
          userId

parseLocation :: [Text] -> Location
parseLocation loc
  | Prelude.length loc == 2 = Location
                                (Prelude.read $ unpack ( (!!) loc 0) :: Double)
                                (Prelude.read $ unpack ( (!!) loc 1) :: Double)
  | otherwise               = Location
                                (Prelude.read "0.00" :: Double)
                                (Prelude.read "0.00" :: Double)
