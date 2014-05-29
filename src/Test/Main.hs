{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, GADTs #-}

module Test.Main where

import Prelude hiding ((++))
import Control.Applicative
import Snap hiding (defaultConfig, get)

import Snap.Test.BDD

import Test.Common

routes = [("/test", writeText html)]

html = "<table><tr><td>One</td><td>Two</td></tr></table>"

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    return App

data App = App


main :: IO ()
main = do
  runSnapTests defaultConfig { reportGenerators = [consoleReport, linuxDesktopReport] } (route routes) app $ do
    tests
  putStrLn ""

tests :: SnapTesting App ()
tests = do
  it "should match selector from a GET request" $ do
    should $ haveSelector <$> get "/test" <*> css "table td"
    shouldNot $ haveSelector <$> get "/test" <*> css "table td.doesntexist"
