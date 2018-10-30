{-# LANGUAGE OverloadedStrings #-}
module ServantSpec (main, spec) where

import           Lib
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec $ spec

spec = with (return app) $ do
  describe "GET users" $ do
    it "respons" $ do
      get "/users" `shouldRespondWith` 200
