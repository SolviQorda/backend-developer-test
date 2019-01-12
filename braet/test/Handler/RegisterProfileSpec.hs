module Handler.RegisterProfileSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "postRegisterProfileR" $ do
        it "returns some basic JSON"
        get RegisterProfileR testID testProfile

        testProfile :: UserProfile
        testProfile =
          UserProfile
            {
            , uid = "13"
            , name = "Solvi"
            , location = ["53.433146", "-2.268468"]
            , games = ["SettlersOfCatan", "Risk"]
            , age = "28"
            , availableToHost = "True"
            , q = ""
            }

        testID :: UserID
        testID = 13
