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

class Monad m => MonadDb m where
  runQuery :: Query -> m [User]

  default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => Query -> m [User]
  runQuery sql = lift $ runQuery sql

instance MonadDb (ReaderT Connection IO) where
  runQuery :: Query -> ReaderT Connection IO [User]
  runQuery sql = do
    conn <- ask
    liftIO $ query_ conn sql

instance MonadDb m => MonadDb (ReaderT r m)
instance MonadDb m => MonadDb (ExceptT a m)
