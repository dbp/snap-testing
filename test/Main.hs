{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, GADTs #-}

module Main where


----------------------------------------------------------
-- Section 0: Imports.                                  --
----------------------------------------------------------
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Snap (Handler, method, Method(..), writeText, writeBS,
             getParam, SnapletInit, makeSnaplet, addRoutes,
             route)
import qualified Snap as Snap

import Control.Concurrent.Async (async)
import qualified System.IO.Streams as Stream
import qualified System.IO.Streams.Concurrent as Stream
import System.Exit (exitSuccess, exitFailure)

import Snap.Test.BDD

----------------------------------------------------------
-- Section 1: Example application used for testing.     --
----------------------------------------------------------
html :: Text
html = "<table><tr><td>One</td><td>Two</td></tr></table>"

routes :: [(ByteString, Handler App App ())]
routes = [("/test", method GET $ writeText html)
         ,("/test", method POST $ writeText "")
         ,("/params", do mq <- getParam "q"
                         writeBS $ fromMaybe "" mq)
         ,("/redirect", Snap.redirect "/test")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    return App

data App = App

----------------------------------------------------------
-- Section 2: Test suite against application.           --
----------------------------------------------------------
tests :: SnapTesting App ()
tests = do
  name "should match selector from a GET request" $ do
    should $ haveSelector <$> get "/test" <*> css "table td"
    shouldNot $ haveSelector <$> get "/test" <*> css "table td.doesntexist"
  name "should not match html on POST request" $
    shouldNot $ haveText <$> post "/test" M.empty <*> val "<html>"
  name "should post parameters" $ do
    should $ haveText <$> post "/params" (params [("q", "hello")]) <*> val "hello"
    shouldNot $ haveText <$> post "/params" (params [("r", "hello")]) <*> val "hello"
  name "basic equality" $ do
    should $ equal <$> val 1 <*> val 1
    should $ equal <$> eval (return 1) <*> val 1
  name "booleans from other tests" $ do
    should $ beTrue <$> val True
    shouldNot $ beTrue <$> val False
  name "status codes" $ do
    name "200" $ do
      should $ succeed <$> get "/test"
      shouldNot $ succeed <$> get "/invalid_url"
    name "404" $ do
      shouldNot $ notfound <$> get "/test"
      should $ notfound <$> get "/invalid_url"
    name "3**" $ do
      should $ redirect <$> get "/redirect"
      shouldNot $ redirect <$> get "/test"
    name "3** with target" $ do
      should $ redirectTo <$> get "/redirect" <*> val "/test"
      shouldNot $ redirectTo <$> get "/redirect" <*> val "/redirect"
      shouldNot $ redirectTo <$> get "/test" <*> val "/redirect"

----------------------------------------------------------
-- Section 3: Code to interface with cabal test.        --
----------------------------------------------------------
main :: IO ()
main = do
  (inp, out) <- Stream.makeChanPipe
  async $ runSnapTests defaultConfig { reportGenerators = [streamReport out, consoleReport] }
                       (route routes)
                       app
                       tests
  res <- Stream.toList inp
  let failing = length $ filter isFailing res
  if failing == 0
     then exitSuccess
     else exitFailure
 where streamReport out results = do res <- Stream.read results
                                     case res of
                                       Nothing -> Stream.write Nothing out
                                       Just r -> do
                                         Stream.write (Just r) out
                                         streamReport out results
       isFailing (TestFail _) = True
       isFailing (TestError _) = True
       isFailing _ = False
