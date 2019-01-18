{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGuAGE DeriveGeneric     #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data InputProfile = InputProfile
  { name            :: Text
  , longitude       :: Double
  , latitude        :: Double
  , games           :: [Text]
  , age             :: Int
  , availableToHost :: Bool
  } deriving (Eq, Show, Generic)


instance FromJSON InputProfile where
  parseJSON (Object v) =
--     -- withObject "InputProfile" $ \v ->
      InputProfile
    <$> v .: "name"
    <*> v .: "longitude"
    <*> v .: "latitude"
    <*> v .: "games"
    <*> v .: "age"
    <*> v .: "availableToHost"
  parseJSON _          = mzero

instance ToJSON UserProfile where
  toJSON (UserProfile playerId name longitude latitude games age availableToHost) =
    object
      [ "name" .= name
      , "id" .= playerId
      --want to cast this to a Double for sorting
      , "longitude" .= longitude
      , "latitude" .= latitude
      , "games" .= games
      , "age" .= age
      , "availableToHost" .= availableToHost
      ]
