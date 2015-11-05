{-# LANGUAGE OverloadedStrings #-}

module Chrome where

import           Control.Applicative  ((<$>))
import           Control.Monad        (forever, mzero)
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Concurrent (forkIO, ThreadId)

import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson           as A
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Text.IO         as T
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text            (Text)
import           Data.List            (isPrefixOf)
import qualified Data.Vector          as V
import           Data.Monoid          ((<>))

import qualified Network.HTTP.Conduit as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets   as WS

import           System.Process (system)
import           System.Exit (ExitCode)
import Control.Concurrent (threadDelay)

-- 1: Start Chrome with remote debuggin protocol enabled
chromeRemoteDebuggingPort :: Int
chromeRemoteDebuggingPort = 9160

startChromeCmd :: String
startChromeCmd = unwords
    [ "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome"
    , "--remote-debugging-port=" <> (show chromeRemoteDebuggingPort)
    -- List here the list of tabs you want to open
    -- , "http://localhost:9160", "http://chromium.org"
    ]

startChrome :: IO ThreadId
startChrome = do
  system "killall \"Google Chrome\""
  threadDelay 1000000
  forkIO (system startChromeCmd >>= print )

-- 2: Get the list of pages
data ChromiumPageInfo = ChromiumPageInfo
    { chromiumDebuggerUrl :: String
    , pageURL :: String
    } deriving (Show)

instance FromJSON ChromiumPageInfo where
    parseJSON (A.Object obj) = ChromiumPageInfo
        <$> obj .: "webSocketDebuggerUrl"
        <*> obj .: "url"
    parseJSON _ = mzero

getChromiumPageInfo :: Int -> IO [ChromiumPageInfo]
getChromiumPageInfo port = do
    request <- Http.parseUrl ("http://localhost:" ++ (show port) ++ "/json")
    manager <- Http.newManager Http.tlsManagerSettings
    response <- Http.httpLbs request manager
    case A.decode (Http.responseBody response) of
            Nothing -> error "getChromiumPageInfo: Parse error"
            Just ci -> return ci
    where


-- Helpers
parseUri :: String -> (String, Int, String)
parseUri uri = fromMaybe (error "parseUri: Invalid URI") $ do
    u    <- Uri.parseURI uri
    auth <- Uri.uriAuthority u
    let port = case Uri.uriPort auth of (':' : str) -> read str; _ -> 80
    return (Uri.uriRegName auth, port, Uri.uriPath u)
