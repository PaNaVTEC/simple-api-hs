{-# LANGUAGE DeriveGeneric #-}

module Data where

import           Data.Aeson.Types
import           Data.Time
import           GHC.Generics

data User = User
  { name              :: String
  , ki                :: Int
  , registration_date :: Day
  } deriving (Eq, Show, Generic)
instance ToJSON User

users :: [User]
users =
    [ User "Goku"         9001 (fromGregorian 1683 3 1)
    , User "Vegeta"       9000 (fromGregorian 1905 1 1)
    , User "Gohan"        5000 (fromGregorian 0195 2 21)
    , User "Piccolo"      4500 (fromGregorian 1925 2 20)
    , User "Krillin"      3500 (fromGregorian 1905 2 22)
    , User "Tien Shinhan" 3000 (fromGregorian 1905 2 21)
    ]
