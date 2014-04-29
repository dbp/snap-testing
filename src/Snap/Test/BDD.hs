{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Snap.Test.BDD
       (
       -- * Types
         SnapTesting
       , TestResult(..)
       , TestResponse(..)
       , SnapTestingConfig (..)

       -- * Configuration
       , defaultConfig

       -- * Running tests
       , runSnapTests
       , consoleReport
       , linuxDesktopReport

       -- * Labeling
       , name

       -- * Applying Predicates
       , should
       , shouldNot

       -- * Helpers for running tests
       , css
       , val

       -- * Getting Responses
       , get
       , get'
       , post
       , params

       -- * Predicates on values
       , equal
       , beTrue

       -- * Predicates on Responses
       , succeed
       , notfound
       , redirect
       , redirectTo
       , haveText
       , haveSelector

       -- * Stateful value tests
       , changes

       -- * Stateful form tests
       , FormExpectations(..)
       , form

       -- * Run actions after block
       , cleanup

       -- * Evaluating arbitrary actions
       , eval

       -- * Create helpers
       , modifySite

       -- * Integrate with QuickCheck
       , quickCheck
       ) where

import           Prelude hiding (FilePath, log)
import           Data.Map (Map)
import qualified Data.Map as M (lookup, mapKeys, empty, fromList)
import           Data.ByteString (ByteString)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T (append, concat, isInfixOf)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Maybe (fromMaybe)
import           Data.List (intercalate, intersperse)

import           Control.Applicative
import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as S (get, put)
import           Control.Exception (SomeException, catch)
import           Control.Concurrent.Async
import           System.Process (system)

import           Snap.Core (Response(..), getHeader)
import           Snap.Snaplet (Handler, SnapletInit, Snaplet)
import           Snap.Test (RequestBuilder, getResponseBody)
import qualified Snap.Test as Test
import           Snap.Snaplet.Test (runHandler', evalHandler', getSnaplet
                                   , closeSnaplet, InitializerState)
import           Test.QuickCheck (Args(..), Result(..), Testable, quickCheckWithResult, stdArgs)

import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Stream
import qualified System.IO.Streams.Concurrent as Stream

import qualified Text.Digestive as DF
import qualified Text.HandsomeSoup as HS
import qualified Text.XML.HXT.Core as HXT

-- | The main type for this library, where `b` is your application state,
-- often called `App`. This is a State monad on top of IO, where the State carries
-- your application (or, more specifically, a top-level handler), and stream of test results
-- to be reported as passing or failing.
type SnapTesting b a = StateT (Handler b b ()
                              , (Snaplet b, InitializerState b)
                              , OutputStream TestResult) IO a

data TestResponse = Html Text | NotFound | Redirect Int Text | Other Int | Empty

data CssSelector = CssSelector Text

data Sentiment a = Positive a | Negative a deriving Show

flipSentiment :: Sentiment a -> Sentiment a
flipSentiment (Positive a) = Negative a
flipSentiment (Negative a) = Positive a

data TestResult = NameStart Text
                 | NameEnd
                 | TestPass (Sentiment Text)
                 | TestFail (Sentiment Text)
                 | TestError Text deriving Show

data SnapTestingConfig = SnapTestingConfig { reportGenerators :: [InputStream TestResult -> IO ()]
                                           }


defaultConfig :: SnapTestingConfig
defaultConfig = SnapTestingConfig { reportGenerators = [consoleReport]
                                  }


-- | dupN duplicates an input stream N times
dupN :: Int -> InputStream a -> IO [InputStream a]
dupN 0 _ = return []
dupN 1 s = return [s]
dupN n s = do (a, b) <- Stream.map (\x -> (x,x)) s >>= Stream.unzip
              rest <- dupN (n - 1) b
              return (a:rest)

-- | Run a set of tests, putting the results through the specified report generators
runSnapTests :: SnapTestingConfig              -- ^ Configuration for test runner
             -> Handler b b ()                 -- ^ Site that requests are run against (often route routes, where routes are your sites routes).
             -> SnapletInit b b                -- ^ Site initializer
             -> SnapTesting b ()               -- ^ Block of tests
             -> IO ()
runSnapTests conf site app tests = do
  (inp, out) <- Stream.makeChanPipe
  let rgs = reportGenerators conf
  istreams <- dupN (length rgs) inp
  consumers <- mapM (\(inp', hndl) -> async (hndl inp')) (zip istreams rgs)
  init <- getSnaplet (Just "test") app
  case init of
    Left err -> error $ show err
    Right (snaplet, initstate) -> do
      evalStateT tests (site, (snaplet, initstate), out)
      Stream.write Nothing out
      mapM_ wait consumers
      closeSnaplet initstate
      return ()


-- | Prints test results to the console. For example:
--
-- > /auth/new_user
-- >  success PASSED
-- >  creates a new account PASSED
consoleReport :: InputStream TestResult -> IO ()
consoleReport stream = cr 0
  where cr indent = do log <- Stream.read stream
                       case log of
                         Nothing -> putStrLn "" >> return ()
                         Just (NameStart n) -> do putStrLn ""
                                                  printIndent indent
                                                  putStr (unpack n)
                                                  cr (indent + indentUnit)
                         Just NameEnd -> cr (indent - indentUnit)
                         Just (TestPass _) -> do putStr " PASSED"
                                                 cr indent
                         Just (TestFail msg) -> do putStr " FAILED\n"
                                                   printMessage indent msg
                                                   cr indent
                         Just (TestError msg) -> do putStr " ERROR("
                                                    putStr (unpack msg)
                                                    putStr ")"
                                                    cr indent
        indentUnit = 2
        printIndent n = putStr (replicate n ' ')
        printMessage n (Positive m) = do printIndent n
                                         putStrLn "Should have held:"
                                         printIndent n
                                         putStrLn (unpack m)
        printMessage n (Negative m) = do printIndent n
                                         putStrLn "Should not have held:"
                                         printIndent n
                                         putStrLn (unpack m)

-- | Sends the test results to desktop notifications on linux.
-- Prints how many tests passed and failed.
linuxDesktopReport :: InputStream TestResult -> IO ()
linuxDesktopReport stream = do
  res <- Stream.toList stream
  let (failing, total) = count [] res
  case failing of
    [] ->
      void $ system $ "notify-send -u low -t 2000 'All Tests Passing' 'All " ++
                       (show total) ++ " tests passed.'"
    _ ->
      void $ system $ "notify-send -u normal -t 2000 'Some Tests Failing' '" ++
                      (show (length failing)) ++ " out of " ++
                      (show total) ++ " tests failed:\n\n" ++ (intercalate "\n\n" $ reverse failing) ++ "'"
 where count :: [Text] -> [TestResult] -> ([String], Int)
       count _ [] = ([], 0)
       count n (TestPass _ : xs) = let (f, t) = count n xs
                                   in (f, 1 + t)
       count n (TestFail _ : xs) = let (f, t) = count n xs
                                   in (f ++ [unpack $ T.concat $ intersperse " > " $ reverse n], 1 + t)
       count n (TestError _ : xs) = let (f, t) = count n xs
                                    in (f, 1 + t)
       count n (NameStart nm : xs) = count (nm:n) xs
       count n (NameEnd : xs) = count (tail n) xs
       count n (_ : xs) = count n xs

writeRes :: TestResult -> SnapTesting b ()
writeRes log = do (_,_,out) <- S.get
                  lift $ Stream.write (Just log) out

-- | Labels a block of tests with a descriptive name, to be used in report generation.
name :: Text              -- ^ Name of block
     -> SnapTesting b ()  -- ^ Block of tests
     -> SnapTesting b ()
name s a = do
  writeRes (NameStart s)
  a
  writeRes NameEnd

runRequest :: RequestBuilder IO () -> SnapTesting b TestResponse
runRequest req = do
  (site, app, _) <- S.get
  res <- liftIO $ runHandlerSafe req site app
  case res of
    Left err -> do
      writeRes (TestError err)
      return $ Empty
    Right response -> do
      case rspStatus response of
        404 -> return NotFound
        200 -> do
          body <- liftIO $ getResponseBody response
          return $ Html $ decodeUtf8 body
        _ -> if (rspStatus response) >= 300 && (rspStatus response) < 400
                then do let url = fromMaybe "" $ getHeader "Location" response
                        return (Redirect (rspStatus response) (decodeUtf8 url))
                else return (Other (rspStatus response))

get :: Text
     -> SnapTesting b TestResponse
get = flip get' M.empty

get' :: Text
     -> Map ByteString [ByteString]
     -> SnapTesting b TestResponse
get' path ps = runRequest (Test.get (encodeUtf8 path) ps)


-- | Creates a new POST request, with a set of parameters.
post :: Text                        -- ^ The url to request.
     -> Map ByteString [ByteString] -- ^ The parameters to send.
     -> SnapTesting b TestResponse
post path ps = runRequest (Test.postUrlEncoded (encodeUtf8 path) ps)

-- | A helper to construct parameters.
params :: [(ByteString, ByteString)] -- ^ Pairs of parameter and value.
       -> Map ByteString [ByteString]
params = M.fromList . map (\x -> (fst x, [snd x]))

css :: Applicative m => Text -> m CssSelector
css = pure . CssSelector

val :: Applicative m => a -> m a
val = pure

should :: SnapTesting b TestResult -> SnapTesting b ()
should test = do res <- test
                 writeRes res

shouldNot :: SnapTesting b TestResult -> SnapTesting b ()
shouldNot test = do res <- test
                    case res of
                      TestPass msg -> writeRes (TestFail (flipSentiment msg))
                      TestFail msg -> writeRes (TestPass (flipSentiment msg))
                      _ -> writeRes res

haveSelector :: TestResponse -> CssSelector -> TestResult
haveSelector (Html body) (CssSelector selector) = case HXT.runLA (HXT.hread HXT.>>> HS.css (unpack selector)) (unpack body)  of
                                                    [] -> TestFail msg
                                                    _ -> TestPass msg
  where msg = (Positive $ T.concat ["Html contains selector: ", selector, "\n\n", body])

haveText :: TestResponse -> Text -> TestResult
haveText (Html body) match =
  if T.isInfixOf match body
  then TestPass (Positive $ T.concat [body, "' contains '", match, "'."])

  else TestFail (Positive $ T.concat [body, "' contains '", match, "'."])
haveText _ match = TestFail (Positive (T.concat ["Body contains: ", match]))


-- | Checks that the handler evaluates to the given value.
equal :: (Show a, Eq a)
      => a
      -> a
      -> TestResult
equal a b = if a == b
               then TestPass (Positive (T.concat [pack $ show a, " == ", pack $ show b]))
               else TestFail (Positive (T.concat [pack $ show a, " == ", pack $ show b]))

-- | Helper to bring the results of other tests into the test suite.
beTrue :: Bool -> TestResult
beTrue True = TestPass (Positive "assertion")
beTrue False = TestFail (Positive "assertion")

-- | A data type for tests against forms.
data FormExpectations a = Value a           -- ^ The value the form should take (and should be valid)
                        | ErrorPaths [Text] -- ^ The error paths that should be populated

-- | Test against digestive-functors forms.
form :: (Eq a, Show a)
     => FormExpectations a           -- ^ If the form should succeed, Value a is what it should produce.
                                     --   If failing, ErrorPaths should be all the errors that are triggered.
     -> DF.Form Text (Handler b b) a -- ^ The form to run
     -> Map Text Text                -- ^ The parameters to pass
     -> SnapTesting b ()
form expected theForm theParams =
  do r <- eval $ DF.postForm "form" theForm (const $ return lookupParam)
     case expected of
       Value a -> should $ equal <$> val (snd r) <*> val (Just a)
       ErrorPaths expectedPaths ->
         do let viewErrorPaths = map (DF.fromPath . fst) $ DF.viewErrors $ fst r
            should $ beTrue <$> val (all (`elem` viewErrorPaths) expectedPaths
                                     && (length viewErrorPaths == length expectedPaths))
  where lookupParam pth = case M.lookup (DF.fromPath pth) fixedParams of
                            Nothing -> return []
                            Just v -> return [DF.TextInput v]
        fixedParams = M.mapKeys (T.append "form.") theParams

-- | Checks that the given request results in a success (200) code.
succeed :: TestResponse -> TestResult
succeed (Html _) = TestPass (Positive "Request 200s.")
succeed _ = TestFail (Positive "Request 200s.")

-- | Checks that the given request results in a not found (404) code.
notfound :: TestResponse -> TestResult
notfound NotFound = TestPass (Positive "Request 404s.")
notfound _ = TestFail (Positive "Request 404s.")

-- | Checks that the given request results in a redirect (3**) code.
redirect :: TestResponse -> TestResult
redirect (Redirect _ _) = TestPass (Positive "Request redirects.")
redirect _ = TestFail (Positive "Request redirects.")

-- | Checks that the given request results in a redirect to a specific url.
redirectTo :: TestResponse -- ^ Request to run
           -> Text         -- ^ URL it should redirect to
           -> TestResult
redirectTo (Redirect _ actual) expected | actual == expected = TestPass (Positive (T.concat ["Redirecting actual: ", actual, " expected: ", expected]))
redirectTo (Redirect _ actual) expected = TestFail (Positive (T.concat ["Redirecting actual: ", actual, " expected: ", expected]))
redirectTo _ expected = TestFail (Positive (T.concat ["Redirects to ", expected]))

-- | Checks that the monadic value given changes by the function specified after the given test block is run.
--
-- For example, if you wanted to make sure that account creation was creating new accounts:
--
-- > changes (+1) countAccounts (post "/auth/new_user" $ params
-- >                             [ ("new_user.name", "Jane")
-- >                             , ("new_user.email", "jdoe@c.com")
-- >                             , ("new_user.password", "foobar")])
changes :: (Show a, Eq a)
        => (a -> a)          -- ^ Change function
        -> Handler b b a     -- ^ Monadic value
        -> SnapTesting b c   -- ^ Test block to run.
        -> SnapTesting b ()
changes delta measure act = do
  before <- eval measure
  _ <- act
  after <- eval measure
  should $ equal <$> val (delta before) <*> val after

-- | Runs an action after a block of tests, usually used to remove database state.
cleanup :: Handler b b ()   -- ^ Action to run after tests
        -> SnapTesting b () -- ^ Tests to run
        -> SnapTesting b ()
cleanup cu act = do
  act
  (_, app, _) <- S.get
  _ <- liftIO $ runHandlerSafe (Test.get "" M.empty) cu app
  return ()

-- | Evaluate arbitrary actions
eval :: Handler b b a -- ^ Action to evaluate
     -> SnapTesting b a
eval act = do
  (_, app, _) <- S.get
  liftIO $ fmap (either (error . unpack) id) $ evalHandlerSafe act app


-- | Given a site to site function (like, generating a random user and logging in), run the given block of test with the modified state.
modifySite :: (Handler b b () -> Handler b b ()) -- ^ Site modification function
           -> SnapTesting b a -- ^ Tests to run
           -> SnapTesting b a
modifySite f act = do
  (site, app, out) <- S.get
  S.put (f site, app, out)
  res <- act
  S.put (site, app, out)
  return res

-- | Allows you to run a quickcheck test. All 100 test passing counts as a pass, any failure a failure.
-- Currently the reporting is really bad (you don't see what the failing example is).
quickCheck :: Testable prop => prop -> SnapTesting b ()
quickCheck p = do
  res <- liftIO $ quickCheckWithResult (stdArgs { chatty = False }) p
  case res of
    Success{} -> writeRes (TestPass (Positive ""))
    GaveUp{} -> writeRes (TestPass (Positive ""))
    Failure{} -> writeRes (TestFail (Positive ""))
    NoExpectedFailure{} -> writeRes (TestFail (Positive ""))

-- Private helpers
runHandlerSafe :: RequestBuilder IO ()
               -> Handler b b v
               -> (Snaplet b, InitializerState b)
               -> IO (Either Text Response)
runHandlerSafe req site (s, is) =
  catch (runHandler' s is req site) (\(e::SomeException) -> return $ Left (pack $ show e))

evalHandlerSafe :: Handler b b v
                -> (Snaplet b, InitializerState b)
                -> IO (Either Text v)
evalHandlerSafe act (s, is) =
  catch (evalHandler' s is (Test.get "" M.empty) act) (\(e::SomeException) -> return $ Left (pack $ show e))
