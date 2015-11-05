{-# LANGUAGE OverloadedStrings #-}

module Watch (monitor) where

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

monitor :: Action -> IO ()
monitor action =
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate
      (\a -> print "doing " >> action a)

    -- sleep forever (until interrupted)
    forever $ threadDelay maxBound