{-# LANGUAGE DeriveGeneric #-}

module Data where

import           Data.Aeson.Types
import           Data.Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

data User = User
  { name              :: String
  , ki                :: Int
  , registration_date :: UTCTime
  } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow user = [
    toField . name $ user,
    toField . ki $ user,
    toField . registration_date $ user]
