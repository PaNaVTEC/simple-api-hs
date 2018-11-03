{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module ServantSpec (main, spec) where

import           Control.Monad.Reader
import           Data
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

newtype TestM a = TestM {
  runTestM :: ReaderT [User] Handler a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader [User])

anAppWith :: Monad m => [User] -> m Application
anAppWith users = return $ app nt
  where
    nt :: TestM a -> Handler a
    nt appM = runReaderT (runTestM appM) users

instance MonadDb TestM where
  runQuery QueryAll = ask

sampleUsers :: [User]
sampleUsers = [User "Goku" 9001 (posixSecondsToUTCTime 0)]
