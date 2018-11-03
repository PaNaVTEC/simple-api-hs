{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module ServantSpec (main, spec) where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data
import           Data.ByteString
import           Data.Time.Clock.POSIX
import           Lib
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec $ spec

spec :: Spec
spec = with (anAppWith sampleUsers) $ do
  describe "GET users" $ do
    it "responds" $ do
      get "/users" `shouldRespondWith` 200
      get "/users" `shouldRespondWith` [json|[{"registration_date":"1970-01-01T00:00:00Z","ki":9001,"name":"Goku"}]|]

type LoggingOutput = [String]
newtype TestM m a = TestM {
  runTestM :: WriterT LoggingOutput (ReaderT [User] m) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader [User])

anAppWith :: Monad m => [User] -> m Application
anAppWith users = return $ app nt
  where
    nt :: TestM Handler a -> Handler a
    nt appM = do
      c <- runReaderT (runWriterT (runTestM appM)) users
      return $ fst c

instance MonadDb (TestM Handler) where
  runQuery QueryAll = ask

instance (Monad m) => MonadLogger (TestM m) where
  monadLoggerLog _ _ _ m = TestM $ tell [show $ toLogStr m]

sampleUsers :: [User]
sampleUsers = [User "Goku" 9001 (posixSecondsToUTCTime 0)]
