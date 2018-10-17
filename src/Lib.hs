module Lib ( startApp ) where

import           Control.Monad.Reader
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Routes
import           Servant

startApp :: (MonadIO m) => m ()
startApp = liftIO $ run 8081 app
  where
    app = serve proxy $ hoistServer proxy nt routes
    proxy = Proxy :: Proxy APIEndpoints

nt :: AppM IO a -> Handler a
nt app = do
  conn <- liftIO $ connect defaultConnectInfo
                { connectDatabase = "sample"
                , connectUser     = "sample"
                , connectPassword = "sample"
                }
  liftIO $ runReaderT app conn
