{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib ( startApp ) where

import           Data.Aeson.Types
import           Data.List
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type GetJson = Get '[JSON]
type APIEndpoints =
  -- /users
  "users" :> GetJson [User]

  -- /users?sortby={ki,name}
  :<|> "users" :> QueryParam "sortby" SortBy :> GetJson [User]

  -- /users/by-ki
  :<|> "users" :> "by-ki" :> GetJson [User]

  -- /user/:username
  :<|> "user" :> Capture "username" String :> GetJson (Maybe User)

data SortBy = Ki | Name
instance FromHttpApiData SortBy where
  parseQueryParam "ki"   = Right Ki
  parseQueryParam "name" = Right Name
  parseQueryParam _      = Left $ "Invalid parameter"

data User = User
  { name              :: String
  , ki                :: Int
  , registration_date :: Day
  } deriving (Eq, Show, Generic)
instance ToJSON User

server :: Server APIEndpoints
server = allUsers :<|> usersBy :<|> usersSortedByKi :<|> userByName

allUsers :: Handler [User]
allUsers = return users

usersBy :: Maybe SortBy -> Handler [User]
usersBy Nothing     = return users
usersBy (Just Ki)   = return (sortOn ki users)
usersBy (Just Name) = return (sortOn name users)

usersSortedByKi :: Handler [User]
usersSortedByKi = return (sortOn ki users)

userByName :: String -> Handler (Maybe User)
userByName userName = return $ Data.List.find byUserName users
  where byUserName user = name user == userName

startApp :: IO ()
startApp = run 8081 app
  where
    app = serve proxy server
    proxy = (Proxy :: Proxy APIEndpoints)

users :: [User]
users =
    [ User "Goku"         9001 (fromGregorian 1683 3 1)
    , User "Vegeta"       9000 (fromGregorian 1905 12 1)
    , User "Gohan"        5000 (fromGregorian 0195 2 21)
    , User "Piccolo"      4500 (fromGregorian 1925 2 20)
    , User "Krillin"      3500 (fromGregorian 1905 2 22)
    , User "Tien Shinhan" 3000 (fromGregorian 1905 2 21)
    ]
