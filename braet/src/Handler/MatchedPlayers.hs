{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs#-}

module Handler.MatchedPlayers where

import Import
import qualified Data.Text as T
import Database.Persist.Sql

getMatchedPlayersR :: Handler Value
getMatchedPlayersR = do
  -- TODO: switch this with selectFirst for safety.
  subject <- getValidGoogleSubject
  user  <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  players <- uncurry (selectHosts (userGames user)) $ userLongLat user
  return $ toJSON $ Prelude.map entityVal players
    where
      selectHosts
        :: [Text]
        -> Double
        -> Double
        -> Handler [Entity HostDetails]
      selectHosts t u v = runDB $ rawSql s [toPersistValue t, toPersistValue u, toPersistValue v]
        where s = "SELECT ?? FROM host_details WHERE games IN ? ORDER BY ST_DISTANCESPHERE(ST_POINT(?,?), ST_POINT(latitude,longitude))"

userLongLat :: [Entity UserProfile] -> (Double, Double)
userLongLat ps =
  ( userProfileLongitude $ entityVal p
  , userProfileLatitude $ entityVal p)
  where p = Prelude.head ps

userGames :: [Entity UserProfile] -> [Text]
userGames ps = userProfileGames $ entityVal p
  where p = Prelude.head ps

instance ToJSON HostDetails where
  toJSON (HostDetails games longitude latitude host _) =
    object
      [ "Games" .= games
      , "Longitude" .= longitude
      , "Latitude" .= latitude
      , "Host" .= host
      ]
