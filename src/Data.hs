{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}

module Data where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import           Data.Aeson.Types
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import Servant

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
  runQuery :: Connection -> Query -> m [User]

  default runQuery :: (MonadDb m', MonadTrans t, t m' ~ m) => Connection -> Query -> m [User]
  runQuery conn sql = lift $ runQuery conn sql

instance MonadDb IO where
  runQuery :: Connection -> Query -> IO [User]
  runQuery conn sql = query_ conn sql

instance MonadDb m => MonadDb (ReaderT r m)
deriving instance MonadDb Handler
instance MonadDb m => MonadDb (ExceptT a m)
