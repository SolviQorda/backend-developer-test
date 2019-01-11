{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}

module Handler.HomeTest (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
--basic test to check landing page is returning data.
  desribe "Home" $ do
    it "loads the api and checks for necessary elements" $ do
      get HomeR
      statusIs 200
