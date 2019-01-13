{-# LANGUAGE OverloadedStrings #-}

module Handler.ShowGames where

import Import
import qualified Data.Text as T

getShowGamesR :: UserId -> Handler Value
getShowGamesR userId = do
  games <- runDB $ selectList [] [Asc HostedGameTitle]
  return $ toJSON $ Prelude.map (\r -> entityVal r) games

instance ToJSON HostedGame where
  toJSON (HostedGame title location host) =
    object
      [ "Title" .= title
      , "Location" .= ((locationLongitude location :: Double),
                       (locationLatitude location  :: Double))
      , "Host" .= host
      ]
