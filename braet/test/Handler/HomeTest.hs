{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}

module Handler.HomeTest (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
--basic test to check landing page is returning data.
  desribe "Landing Page" $ do
    it "loads the index and checks for necessary elements" $ do
      get HomeR
      statusIs 200
      htmlAllContain "h2" "Let's get started!"
