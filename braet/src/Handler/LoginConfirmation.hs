{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.LoginConfirmation where

import Import
import qualified Data.Text as T

getLoginConfirmationR :: Handler Value
getLoginConfirmationR = do
  (authId, user) <- requireAuthPair
  -- return $ toJSON user
  sendResponseStatus status201 ("AUTHENTICATED SUCCESSFULLY" :: Text)
  -- redirect $ RegisterProfileR

instance ToJSON User where
  toJSON (User email) =
    object
      [ "Email" .= email ]
