{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs#-}

module Handler.ShowGames where

import Import
import qualified Data.Text as T
import Database.Persist.Sql

getShowGamesR :: Handler Value
getShowGamesR = do
  (authId, user) <- requireAuthPair
  games <- runDB $ selectList [HostedGameHostId ==. authId] [Asc HostedGameTitle]
  return $ toJSON $ Prelude.map (\r -> entityVal r) games

-- getShowGamesR :: UserId -> Handler Value
-- getShowGamesR userId = do
--   -- TODO: switch this with selectFirst for safety.
--   user  <- runDB $ selectList [UserProfilePlayerId ==. (pack $ show userId)] [Asc UserProfilePlayerId]
--   games <- selectGames (userGames user) (fst $ userLongLat user) (snd $ userLongLat user)
--   return $ toJSON $ Prelude.map entityVal $ games
--     where
--       selectGames
--         :: [Text]
--         -> Double
--         -> Double
--         -> Handler [Entity HostedGame]
--       selectGames t u v = runDB $ rawSql s [toPersistValue t, toPersistValue u, toPersistValue v]
--         where s = "SELECT ?? FROM hosted_game WHERE title IN ? ORDER BY ST_DISTANCESPHERE(ST_POINT(?,?), ST_POINT(latitude,longitude))"

userLongLat :: [Entity UserProfile] -> (Double, Double)
userLongLat ps =
  ( userProfileLongitude $ entityVal p
  , userProfileLatitude $ entityVal p)
  where p = Prelude.head ps

userGames :: [Entity UserProfile] -> [Text]
userGames ps = userProfileGames $ entityVal p
  where p = Prelude.head ps

instance ToJSON HostedGame where
  toJSON (HostedGame title longitude latitude host hostId) =
    object
      [ "Title" .= title
      , "Longitude" .= longitude
      , "Latitude" .= latitude
      , "Host" .= host
      ]
