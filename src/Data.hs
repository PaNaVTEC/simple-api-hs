{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data where

--import           Control.Monad.Trans
import           Control.Monad.Reader
import           Data.Aeson.Types
import           Data.Time
import           Database.PostgreSQL.Simple
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

class Monad m => MonadDb m where
  runQuery :: Connection -> Query -> m [User]

--  default runQuery :: (MonadTrans t, MonadDb m', m ~ t m') => Query -> m [User]
--  runQuery = lift . runQuery

--  default runCommand :: (MonadTrans t, MonadDatabase m', m ~ t m') => Command -> m ()
--  runCommand = lift . runCommand

instance (Monad m, MonadIO m) => MonadDb m where
  runQuery :: Connection -> Query -> m [User]
  runQuery conn sql = liftIO $ query_ conn sql

--instance MonadDb m => MonadDb (ReaderT r m) where
--  runQuery :: Connection -> Query -> ReaderT r m [User]
--  runQuery conn sql = lift $ runQuery conn sql
