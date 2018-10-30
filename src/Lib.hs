{-# LANGUAGE FlexibleContexts #-}

module Lib ( startApp, app ) where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Routes
import           Servant

startApp :: (MonadReader Connection m, MonadIO m) => m ()
startApp = do
  conn <- ask
  liftIO $ run 8081 (app conn)

proxy :: Proxy APIEndpoints
proxy = (Proxy :: Proxy APIEndpoints)

app :: Connection -> Application
app conn = serve proxy $ hoistServer proxy (nt conn) routes

nt :: Connection -> (AppM Handler a -> Handler a)
nt = \conn -> \app -> runReaderT (runAppM app) conn
