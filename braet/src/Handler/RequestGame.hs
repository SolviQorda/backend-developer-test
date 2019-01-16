{-# LANGUAGE OverloadedStrings #-}
{-# LANGuAGE DeriveGeneric     #-}

module Handler.RequestGame where

import Import
import Data.Aeson
import GHC.Generics

-- should this be Value -> Handler Value
postRequestGameR :: Value -> Handler Value
postRequestGameR hostedGame = do
  authId  <- requireAuthId
  -- joinGameRequest <- runDB $ insert $ JoinGameRequest authId (hostedGameId $ fromJSON hostedGame)
  -- return $ toJSON joinGameRequest
  redirect $ ShowGamesR

instance ToJSON JoinGameRequest where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HostedGame
