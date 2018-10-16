{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes ( routes, APIEndpoints, AppM ) where

import           Control.Monad.Reader
import           Data
import           Data.List
import           Database.PostgreSQL.Simple
import           Servant

type GetJson = Get '[JSON]
type AppM = ReaderT Connection Handler

type APIEndpoints =
  -- /users
  "users" :> GetJson [User]

  -- /users?sortby={ki,name}
  :<|> "users" :> QueryParam "sortby" SortBy :> GetJson [User]

  -- /users/by-ki
  :<|> "users" :> "by-ki" :> GetJson [User]

  -- /user/:username
  :<|> "user" :> Capture "username" String :> GetJson (Maybe User)

routes :: ServerT APIEndpoints AppM
routes = allUsers :<|> usersBy :<|> usersSortedByKi :<|> userByName

data SortBy = Ki | Name
instance FromHttpApiData SortBy where
  parseQueryParam "ki"   = Right Ki
  parseQueryParam "name" = Right Name
  parseQueryParam _      = Left $ "Invalid parameter"

allUsers :: AppM [User]
allUsers = do
  conn <- ask
  liftIO $ query_ conn "SELECT * FROM users"

usersBy :: Maybe SortBy -> AppM [User]
usersBy Nothing     = allUsers
usersBy (Just Ki)   = do
  users <- allUsers
  return (sortOn ki users)
usersBy (Just Name) = do
  users <- allUsers
  return (sortOn name users)

usersSortedByKi :: AppM [User]
usersSortedByKi = do
  users <- allUsers
  return (sortOn ki users)

userByName :: String -> AppM (Maybe User)
userByName userName = do
  users <- allUsers
  return $ find byUserName users
  where byUserName user = name user == userName
