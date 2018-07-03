{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes ( routes, APIEndpoints ) where

import           Data
import           Data.List
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

routes :: Server APIEndpoints
routes = allUsers :<|> usersBy :<|> usersSortedByKi :<|> userByName

data SortBy = Ki | Name
instance FromHttpApiData SortBy where
  parseQueryParam "ki"   = Right Ki
  parseQueryParam "name" = Right Name
  parseQueryParam _      = Left $ "Invalid parameter"

allUsers :: Handler [User]
allUsers = return users

usersBy :: Maybe SortBy -> Handler [User]
usersBy Nothing     = return users
usersBy (Just Ki)   = return (sortOn ki users)
usersBy (Just Name) = return (sortOn name users)

usersSortedByKi :: Handler [User]
usersSortedByKi = return (sortOn ki users)

userByName :: String -> Handler (Maybe User)
userByName userName = return $ find byUserName users
  where byUserName user = name user == userName
