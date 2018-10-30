module Main where

import           Lib
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Control.Monad.Reader

main :: IO ()
main = undefined

prodConn :: IO Connection
prodConn = connect defaultConnectInfo
             { connectDatabase = "sample"
             , connectUser     = "sample"
             , connectPassword = "sample"
             }
