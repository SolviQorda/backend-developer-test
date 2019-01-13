
module Handler.RequestGame where

import Import

postRequestGameR :: UserId -> HostedGameId -> Handler Value
postRequestGameR userId hostedGameId = do
  _ <- runDB $ insert (JoinGameRequest userId hostedGameId)
  redirect $ ShowGamesR userId
