{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

module Routes ( routes, APIEndpoints, AppM(..), AppT ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data
import           Data.List
import           Database.PostgreSQL.Simple
import           Servant

type GetJson = Get '[JSON]

type AppT a = AppM Handler a
newtype AppM m a = AppM {
  runAppM :: ReaderT Connection m a
} deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO, MonadDb)

instance MonadTrans AppM where
  lift :: Monad m => m a -> AppM m a
  lift ma = AppM . lift $ ma

type APIEndpoints =
  -- /users
  "users" :> GetJson [User]

  -- /users?sortby={ki,name}
  :<|> "users" :> QueryParam "sortby" SortBy :> GetJson [User]

  -- /users/by-ki
  :<|> "users" :> "by-ki" :> GetJson [User]

  -- /user/:username
  :<|> "user" :> Capture "username" String :> GetJson (Maybe User)

routes :: (MonadReader Connection m, MonadDb m) => ServerT APIEndpoints m
routes = allUsers :<|> usersBy :<|> usersSortedByKi :<|> userByName

data SortBy = Ki | Name
instance FromHttpApiData SortBy where
  parseQueryParam "ki"   = Right Ki
  parseQueryParam "name" = Right Name
  parseQueryParam _      = Left $ "Invalid parameter"

allUsers :: (MonadReader Connection m, MonadDb m) => m [User]
allUsers = do
  conn <- ask
  runQuery conn "SELECT * FROM users"

usersBy :: (MonadReader Connection m, MonadDb m) => Maybe SortBy -> m [User]
usersBy Nothing     = allUsers
usersBy (Just Ki)   = do
  users <- allUsers
  return (sortOn ki users)
usersBy (Just Name) = do
  users <- allUsers
  return (sortOn name users)

usersSortedByKi :: (MonadReader Connection m, MonadDb m) => m [User]
usersSortedByKi = do
  users <- allUsers
  return (sortOn ki users)

userByName :: (MonadReader Connection m, MonadDb m) => String -> m (Maybe User)
userByName userName = do
  users <- allUsers
  return $ find byUserName users
  where byUserName user = name user == userName
