{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

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
import Control.Monad (when)
import qualified Network.HTTP.Conduit as Http
import qualified Network.URI          as Uri
import qualified Network.WebSockets   as WS

import           System.Process (system)
import           System.Exit (ExitCode)

import CmdArgs
import Options.Applicative

import Watch (monitor)
import Chrome
import Commands

opts :: ParserInfo CmdArgs
opts = info (helper <*> cmdArgs)
        (fullDesc
         <> progDesc "For WAI complaint haskell web applications"
         <> header "Reloader: Reload chrome tabs when files change locally." )

main :: IO ()
main = do
    CmdArgs shouldRestart website files <- execParser opts
    when shouldRestart (startChrome >> return ())

    pages <- getChromiumPageInfo 9160
    putStrLn "" >> putStrLn "pages:"
    print pages
    let (ci : _) = filter (\page -> isPrefixOf website (pageURL page)) pages

    putStrLn "" >> putStrLn "ci:"
    print ci

    -- putStrLn "" >> putStrLn "request"
    -- LBS.putStrLn $ A.encode $ searchName "remi"
    --
    let (host, port, path) = parseUri (chromiumDebuggerUrl ci)
    WS.runClient host port path $ \conn -> do
        forkIO (monitor (const (WS.sendTextData conn $ A.encode reload)) >> return ())
        WS.sendTextData conn $ A.encode reload
        forever $ do
            msg <- WS.receiveData conn
            liftIO $ do
                putStrLn "------------------\nresult:"
                T.putStrLn msg
                putStrLn "------------------"
            txt <- getLine
            let cmd = A.encode reload
            print cmd
            WS.sendTextData conn cmd
