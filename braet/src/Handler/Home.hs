{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Value
getHomeR = do
    return placeholderValue
        -- aDomId <- newIdent
        -- setTitle "Welcome To Braet!"

postHomeR :: Handler Value
postHomeR = do
  return placeholderValue
        -- aDomId <- newIdent
        -- setTitle "Welcome To Braet!"
        -- $(widgetFile "homepage")

placeholderValue :: Value
placeholderValue = object
  [ ("braet" .= ("spil" :: String))]
