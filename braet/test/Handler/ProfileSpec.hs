{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}

module Handler.ProfileTest (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
--basic test to check landing page is returning data.
  desribe "Profile" $ do
    it "loads the api and checks for profile" $ do
      get ProfileR
      statusIs 200
