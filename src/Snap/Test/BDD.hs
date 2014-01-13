{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

{-|

This library allows you to write tests against handlers, checking
response codes and bodies, modifications of state, etc. The tests are written
in a hierarchical fashion, with labels to help organize them, and
various ways of reporting on the results of testing are possible.

All of the tests are run in the "test" environment, so be sure to
create the corresponding .cfg files and databases, etc.

Here is a complete example (where routes are your applications routes, and
app is your site initializer):

> runSnapTests [consoleReport, desktopReport] (route routes) app $ do
>   name "/auth/new_user" $ do
>     name "success" $
>       succeeds (get "/auth/new_user")
>     name "creates a new account" $
>       cleanup clearAccounts $
>       changes (+1) countAccounts (post "/auth/new_user" $ params
>                                   [ ("new_user.name", "Jane")
>                                   , ("new_user.email", "jdoe@c.com")
>                                   , ("new_user.password", "foobar")])

There are many different predicates available (and a basic way of
integrating QuickCheck), and it is relatively easy to add
functionality on top of what is built in. For example, to add a way of
creating users and logging in as them for a block of tests you could
do the following (this is using the auth snaplet - if you are doing
somethinge else, obviously the `with auth ...` line would be
different):

> withUser :: SnapTesting App a -> SnapTesting App a
> withUser = modifySite $ \site -> do
>   au <- fmap fromJust getRandomUser
>   with auth $ forceLogin au
>   site

Where `getRandomUser` is a function written in your applications
handler (using whatever state needed).

-}
module Snap.Test.BDD
       (
       -- * Types
         SnapTesting
       , TestRequest
       , TestResult(..)

       -- * Running tests
       , runSnapTests
       , consoleReport
       , linuxDesktopReport

       -- * Labeling
       , name

       -- * Creating Requests
       , get
       , post
       , params

       -- * Request predicates
       , succeeds
       , notfound
       , redirects
       , redirectsto
       , changes
       , changes'
       , contains
       , notcontains

       -- * Stateful unit tests
       , equals

       -- * Run actions after block
       , cleanup

       -- * Evaluate arbitrary action
       , eval

       -- * Create helpers
       , modifySite

       -- * Integrate with QuickCheck
       , quickCheck
       ) where

import           Data.Map (Map, fromList)
import           Data.ByteString (ByteString, isInfixOf)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T (append)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Monoid (mempty)
import           Data.Maybe (fromJust)
import           Control.Monad (liftM, zipWithM, void)
import           Control.Monad.Trans
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as S (get, put)
import           Control.Monad.Trans.Writer (WriterT(..), tell)
import           Control.Exception (SomeException, catch)
import           System.Process (system)
import           Snap.Core (Response(..), getHeader)
import           Snap.Snaplet (Handler, SnapletInit)
import           Snap.Test (RequestBuilder, getResponseBody)
import qualified Snap.Test as Test
import           Snap.Snaplet.Test (runHandler, evalHandler)
import           Test.QuickCheck (Args(..), Result(..), Testable, quickCheckWithResult, stdArgs)

-- | The main type for this library, where `b` is your application state,
-- often called `App`. This is a State and Writer monad on top of IO, where the State carries
-- your application (or, more specifically, a top-level handler), and the Writer allows tests
-- to be reported as passing or failing.
type SnapTesting b a = WriterT [TestLog] (StateT (Handler b b (), SnapletInit b b) IO) a

-- | TestRequests are created with `get` and `post`.
type TestRequest = RequestBuilder IO ()

-- | TestResults are what are used to write report generators (two are included). The result
-- is a tree structure.
data TestResult = ResultName Text [TestResult] | ResultPass Text | ResultFail Text
-- TestLog is the flat datastructure that will be turned into the TestResult tree
data TestLog = NameStart Text | NameEnd | TestPass Text | TestFail Text deriving Show

-- | Run a set of tests, putting the results through the specified report generators
runSnapTests :: [[TestResult] -> IO ()] -- ^ Report generators
                -> Handler b b ()       -- ^ Site that requests are run against (often route routes, where routes are your sites routes).
                -> SnapletInit b b      -- ^ Site initializer
                -> SnapTesting b ()     -- ^ Block of tests
                -> IO ()
runSnapTests rgs site app tests = do
  testlog <- liftM snd $ evalStateT (runWriterT tests) (site, app)
  let res = fst $ buildResult [] testlog
  _ <- zipWithM ($) rgs (repeat res)
  return ()

buildResult :: [TestResult] -> [TestLog] -> ([TestResult], [TestLog])
buildResult acc [] = (acc, [])
buildResult acc ((NameStart nm):xs) =
  let (cur, rest) = buildResult [] xs in
  buildResult (acc ++ [(ResultName nm cur)]) rest
buildResult acc (NameEnd:xs) = (acc, xs)
buildResult acc ((TestPass desc):xs) = buildResult (acc ++ [ResultPass desc]) xs
buildResult acc ((TestFail desc):xs) = buildResult (acc ++ [ResultFail desc]) xs

-- | Prints test results to the console. For example:
--
-- > /auth/new_user
-- >  success
-- >    PASSED
-- >  creates a new account
-- >    PASSED
consoleReport :: [TestResult] -> IO ()
consoleReport = cg 0
  where cg _ [] = return ()
        cg indent (ResultName n children : xs) = do
          fmt indent n
          cg (indent + 2) children
          cg indent xs
        cg indent (ResultPass n : xs) = do
          fmt indent (T.append "PASSED " n)
          cg indent xs
        cg indent (ResultFail n : xs) = do
          fmt indent (T.append "FAILED: " n)
          cg indent xs
        fmt indent t = putStrLn $ replicate indent ' ' ++ unpack t

-- | Sends the test results to desktop notifications on linux. Prints how many tests passed and failed.
linuxDesktopReport ::  [TestResult] -> IO ()
linuxDesktopReport res = do
  let (passed, total) = count res
  case passed == total of
    True ->
      void $ system $ "notify-send -u low -t 2000 'All Tests Passing' 'All " ++ (show total) ++ " tests passed.'"
    False ->
      void $ system $ "notify-send -u normal -t 2000 'Some Tests Failing' '" ++ (show (total - passed)) ++ " out of " ++ (show total) ++ " tests failed.'"
 where count [] = (0, 0)
       count (ResultName _ children : xs) = count (children ++ xs)
       count (ResultPass _ : xs) = let (p, t) = count xs
                                   in (1 + p, 1 + t)
       count (ResultFail _ : xs) = let (p, t) = count xs
                                   in (p, 1 + t)

-- | Labels a block of tests with a descriptive name, to be used in report generation.
name :: Text              -- ^ Name of block
     -> SnapTesting b ()  -- ^ Block of tests
     -> SnapTesting b ()
name s a = do
  tell [NameStart s]
  a
  tell [NameEnd]

-- | Creates a new GET request.
get :: ByteString -- ^ The url to request.
    -> TestRequest
get = flip Test.get mempty

-- | Creates a new POST request, with a set of parameters.
post :: ByteString                  -- ^ The url to request.
     -> Map ByteString [ByteString] -- ^ The parameters to send.
     -> TestRequest
post = Test.postUrlEncoded

-- | A helper to construct parameters.
params :: [(ByteString, ByteString)] -- ^ Pairs of parameter and value.
       -> Map ByteString [ByteString]
params = fromList . map (\x -> (fst x, [snd x]))

-- | Checks that the handler evaluates to the given value.
equals :: (Show a, Eq a) => a -- ^ Value to compare against
          -> Handler b b a    -- ^ Handler that should evaluate to the same thing
          -> SnapTesting b ()
equals a ha = do
  b <- eval ha
  res <- testEqual "Expected value to equal " a b
  tell [res]

-- | Checks that the given request results in a success (200) code.
succeeds :: TestRequest -> SnapTesting b ()
succeeds req = run req testSuccess

-- | Checks that the given request results in a not found (404) code.
notfound :: TestRequest -> SnapTesting b ()
notfound req = run req test404

-- | Checks that the given request results in a redirect (3**) code.
redirects :: TestRequest -> SnapTesting b ()
redirects req = run req testRedirect

-- | Checks that the given request results in a redirect to a specific url.
redirectsto :: TestRequest -- ^ Request to run
            -> Text        -- ^ URL it should redirect to
            -> SnapTesting b ()
redirectsto req uri = run req (testRedirectTo $ encodeUtf8 uri)

-- | Checks that the monadic value given changes by the function specified after the request is run.
--
-- For example, if you wanted to make sure that account creation was creating new accounts:
--
-- > changes (+1) countAccounts (post "/auth/new_user" $ params
-- >                             [ ("new_user.name", "Jane")
-- >                             , ("new_user.email", "jdoe@c.com")
-- >                             , ("new_user.password", "foobar")])
changes :: (Show a, Eq a) =>
           (a -> a)      -- ^ Change function
        -> Handler b b a -- ^ Monadic value
        -> TestRequest   -- ^ Request to run.
        -> SnapTesting b ()
changes delta measure req = do
  (site, app) <- lift S.get
  changes' delta measure (liftIO $ runHandlerSafe req site app)

-- | A more general variant of `changes` that allows an arbitrary block instead of a request.
changes' :: (Show a, Eq a) =>
            (a -> a)        -- ^ Change function
         -> Handler b b a   -- ^ Monadic value
         -> SnapTesting b c -- ^ Block of tests to run
         -> SnapTesting b ()
changes' delta measure act = do
  before <- eval measure
  _ <- act
  after <- eval measure
  res <- testEqual "Expected value to change" (delta before) after
  tell [res]

-- | Checks that the response body of a given request contains some text.
contains :: TestRequest -- ^ Request to run
         -> Text        -- ^ Text that body should contain
         -> SnapTesting b ()
contains req mtch = run req (testBodyContains (encodeUtf8 mtch))

-- | Checks that the response body of a given request does not contain some text.
notcontains :: TestRequest -- ^ Request to run
            -> Text        -- ^ Text that body should not contain
            -> SnapTesting b ()
notcontains req mtch = run req (testBodyNotContains (encodeUtf8 mtch))

-- | Runs an action after a block of tests, usually used to remove database state.
cleanup :: Handler b b ()   -- ^ Action to run after tests
        -> SnapTesting b () -- ^ Tests to run
        -> SnapTesting b ()
cleanup cu act = do
  act
  (_, app) <- lift S.get
  _ <- liftIO $ runHandlerSafe (get "") cu app
  return ()

-- | Evaluate arbitrary actions
eval :: Handler b b a -- ^ Action to evaluate
     -> SnapTesting b a
eval act = do
  (_, app) <- lift S.get
  liftIO $ fmap (either (error. unpack) id) $ evalHandlerSafe act app


-- | Given a site to site function (like, generating a random user and logging in), run the given block of test with the modified state.
modifySite :: (Handler b b () -> Handler b b ()) -- ^ Site modification function
           -> SnapTesting b a -- ^ Tests to run
           -> SnapTesting b a
modifySite f act = do
  (site, app) <- lift S.get
  lift $ S.put (f site, app)
  res <- act
  lift $ S.put (site, app)
  return res

-- | Allows you to run a quickcheck test. All 100 test passing counts as a pass, any failure a failure.
-- Currently the reporting is really bad (you don't see what the failing example is).
quickCheck :: Testable prop => prop -> SnapTesting b ()
quickCheck p = do
  res <- liftIO $ quickCheckWithResult (stdArgs { chatty = False }) p
  case res of
    Success{} -> tell [TestPass ""]
    GaveUp{} -> tell [TestPass ""]
    Failure{} -> tell [TestFail ""]
    NoExpectedFailure{} -> tell [TestFail ""]

-- Private helpers
runHandlerSafe :: TestRequest -> Handler b b v -> SnapletInit b b -> IO (Either Text Response)
runHandlerSafe req site app =
  catch (runHandler (Just "test") req site app) (\(e::SomeException) -> return $ Left (pack $ show e))

evalHandlerSafe :: Handler b b v -> SnapletInit b b -> IO (Either Text v)
evalHandlerSafe act app =
  catch (evalHandler (Just "test") (get "") act app) (\(e::SomeException) -> return $ Left (pack $ show e))


run :: TestRequest -> (Response -> SnapTesting b TestLog) -> SnapTesting b ()
run req asrt = do
  (site, app) <- lift S.get
  res <- liftIO $ runHandlerSafe req site app
  case res of
    Left err -> tell [TestFail $ T.append "Handler returned an error: " err]
    Right response -> do
      testlog <- asrt response
      tell [testlog]

-- Low level matchers - these parallel HUnit assertions in Snap.Test

testEqual :: (Eq a, Show a) => Text -> a -> a -> SnapTesting b TestLog
testEqual msg a b = return $ if a == b then TestPass "" else TestFail msg

testBool :: Text -> Bool -> SnapTesting b TestLog
testBool msg b = return $ if b then TestPass "" else TestFail msg

testSuccess :: Response -> SnapTesting b TestLog
testSuccess rsp = testEqual message 200 status
  where
    message = pack $ "Expected success (200) but got (" ++ show status ++ ")"
    status  = rspStatus rsp

test404 :: Response -> SnapTesting b TestLog
test404 rsp = testEqual message 404 status
  where
    message = pack $ "Expected Not Found (404) but got (" ++ show status ++ ")"
    status = rspStatus rsp

testRedirectTo :: ByteString
                  -> Response
                  -> SnapTesting b TestLog
testRedirectTo uri rsp = do
    testRedirect rsp
    testEqual message uri rspUri
  where
    rspUri = fromJust $ getHeader "Location" rsp
    message = pack $ "Expected redirect to " ++ show uri
              ++ " but got redirected to "
              ++ show rspUri ++ " instead"

testRedirect :: Response -> SnapTesting b TestLog
testRedirect rsp = testBool message (300 <= status && status <= 399)
  where
    message = pack $ "Expected redirect but got status code ("
              ++ show status ++ ")"
    status  = rspStatus rsp


containsGen :: (Bool -> Bool) -> Text -> ByteString -> Response -> SnapTesting b TestLog
containsGen b message match rsp =
  do
    body <- liftIO $ getResponseBody rsp
    return $ if b (match `isInfixOf` body) then TestPass "" else TestFail message

testBodyContains :: ByteString
                -> Response
                -> SnapTesting b TestLog
testBodyContains match = containsGen id message match
  where
    message = pack $ "Expected body to contain \"" ++ show match
              ++ "\", but didn't"


testBodyNotContains :: ByteString
                   -> Response
                   -> SnapTesting b TestLog
testBodyNotContains match = containsGen not message match
  where
    message = pack $ "Expected body to not contain \"" ++ show match
              ++ "\", but did"
