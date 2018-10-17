{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Data where

--import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.Reader
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

instance MonadDb m => MonadDb (ReaderT r m)
instance MonadDb m => MonadDb (ExceptT r m)

deriving instance MonadDb Handler

class Monad m => MonadDb m where
  runQuery :: Connection -> Query -> m [User]

  default runQuery :: (MonadTrans t, MonadDb m', m ~ t m') => Connection -> Query -> m [User]
  runQuery x y = lift $ runQuery x y

--  default runCommand :: (MonadTrans t, MonadDatabase m', m ~ t m') => Command -> m ()
--  runCommand = lift . runCommand

instance MonadDb IO where
  runQuery :: Connection -> Query -> IO [User]
  runQuery = query_

--instance MonadDb m => MonadDb (ReaderT r m) where
--  runQuery :: Connection -> Query -> ReaderT r m [User]
--  runQuery conn sql = lift $ runQuery conn sql
