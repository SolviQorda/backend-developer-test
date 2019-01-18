{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs#-}

module Handler.MatchedPlayers where

import Import
import Database.Persist.Sql

getMatchedPlayersR :: Handler Value
getMatchedPlayersR = do
  subject <- getValidGoogleSubject
  user  <- runDB $ selectList [UserProfilePlayerId ==. subject] [Asc UserProfilePlayerId]
  players <- runDB $ rawSql s [toPersistValue subject, toPersistValue (fst $ userLongLat user), toPersistValue (snd $ userLongLat user)]
  return $ toJSON $ Prelude.map entityVal (players :: [Entity UserProfile])
  where s = "SELECT ?? FROM user_profile WHERE games IN (SELECT games FROM user_profile WHERE player_id = ?) ORDER BY ST_DISTANCESPHERE(ST_POINT(?,?), ST_POINT(longitude,latitude))"

userLongLat :: [Entity UserProfile] -> (Double, Double)
userLongLat ps =
  ( userProfileLongitude $ entityVal p
  , userProfileLatitude $ entityVal p)
  where p = Prelude.head ps

userGames :: [Entity UserProfile] -> [Text]
userGames ps = userProfileGames $ entityVal p
  where p = Prelude.head ps
