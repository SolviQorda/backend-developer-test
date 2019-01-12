{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

getProfileR :: Handler Value
getProfileR = do
  (_, user) <- requireAuthPair
  return $ toJSON user

-- User -> Value

instance ToJSON User where
  toJSON (User email) =
    object
      [ "Email" .= email ]
