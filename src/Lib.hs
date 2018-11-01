{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Lib ( startApp, app ) where

import           Control.Monad.Reader
import           Data
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Routes
import           Servant

startApp :: (MonadReader Connection m, MonadIO m) => m ()
startApp = do
  conn <- ask
  liftIO $ run 8081 (app (nt' conn))

proxy :: Proxy APIEndpoints
proxy = (Proxy :: Proxy APIEndpoints)

app :: MonadDb m => (forall a. m a -> Handler a) -> Application
app nt = serve proxy $ hoistServer proxy nt routes

nt' :: Connection -> (AppT a -> Handler a)
nt' conn app' = runReaderT (runAppM app') conn
