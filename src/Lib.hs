module Lib ( startApp ) where

import           Network.Wai.Handler.Warp
import           Routes                   (APIEndpoints, routes)
import           Servant

startApp :: IO ()
startApp = run 8081 app
  where
    app = serve proxy routes
    proxy = (Proxy :: Proxy APIEndpoints)
