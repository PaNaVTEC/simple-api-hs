{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data where

import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.Aeson.Types
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Servant

newtype DbContext m a = DbContext {
  runDbContext :: (ReaderT Connection m a)
} deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO, MonadDb)

data User = User {
  name                :: String
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

data DbQuery = QueryAll deriving Show

class Monad m => MonadDb m where
  runQuery :: DbQuery -> m [User]

  default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => DbQuery -> m [User]
  runQuery q = lift $ runQuery q

instance MonadIO m => MonadDb (ReaderT Connection m) where
  runQuery :: DbQuery -> ReaderT Connection m [User]
  runQuery q = do
    conn <- ask
    liftIO $ query_ conn (toSql q)
    where
      toSql QueryAll = "select * from users"

instance MonadDb m => MonadDb (ExceptT a m)
