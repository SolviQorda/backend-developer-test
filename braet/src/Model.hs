{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data UserProfile =
  UserProfile
    { uid             :: Text
    , name            :: Text
    , location        :: [Text]
    , games           :: [Text]
    , age             :: Text
    , availableToHost :: Text
    , q               :: [Text]
    } deriving (Eq, Show, Read)

instance PathMultiPiece UserProfile where
  toPathMultiPiece
    (UserProfile uid name location games age availableToHost q)
      = uid : name : (T.unwords location) : (T.unwords games) : age : availableToHost : q

  fromPathMultiPiece
    ( uid : name : location : games : age : availableToHost : q)
      = Just $ UserProfile uid name (T.words location) (T.words games) age availableToHost q
