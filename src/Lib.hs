{-# LANGUAGE FlexibleContexts #-}

module Lib ( startApp ) where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Routes
import           Servant

startApp :: (MonadReader Connection m, MonadIO m) => m ()
startApp = do
  nt' <- nt
  liftIO $ run 8081 (app nt')
  where
    app :: (AppM Handler a -> Handler a) -> Application
    app n = serve proxy $ hoistServer proxy n routes
    proxy = (Proxy :: Proxy APIEndpoints)

nt :: (MonadReader Connection m) => m (AppM Handler a -> Handler a)
nt = do
  conn <- ask
  return $ \app -> runReaderT (runAppM app) conn

prodConn :: IO Connection
prodConn = connect defaultConnectInfo
             { connectDatabase = "sample"
             , connectUser     = "sample"
             , connectPassword = "sample"
             }
