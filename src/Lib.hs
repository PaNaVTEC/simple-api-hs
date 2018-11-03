{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Lib ( startApp , app ) where

import           Control.Monad.Reader
import           Data
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Routes
import           Servant

-- ServantApp with abstract database access in IO

proxy :: Proxy APIEndpoints
proxy = (Proxy :: Proxy APIEndpoints)

app :: MonadDb m => (forall a. m a -> Handler a) -> Application
app nt = serve proxy $ hoistServer proxy nt routes

-- ServantApp wrapped with postgress in another layer of IO

startApp :: IO ()
startApp = do
  conn <- prodConn
  run 8081 (app (ntAppT conn))

ntAppT :: Connection -> AppT a -> Handler a
ntAppT conn appT = runReaderT (runDbContext (runAppM appT)) conn

prodConn :: IO Connection
prodConn = connect defaultConnectInfo
             { connectDatabase = "sample"
             , connectUser     = "sample"
             , connectPassword = "sample"
             }
