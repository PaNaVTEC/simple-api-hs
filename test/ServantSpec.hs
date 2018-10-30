{-# LANGUAGE OverloadedStrings #-}
module ServantSpec (main, spec) where

import           Lib
import           Servant
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec $ spec

spec = with (return $ app undefined) $ do
  describe "GET users" $ do
    it "responds" $ do
      get "/users" `shouldRespondWith` 200
