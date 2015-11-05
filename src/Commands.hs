{-# LANGUAGE OverloadedStrings #-}
module Commands where

import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson           as A
import Chrome
import qualified Data.Map             as M

data Command = Command
    { commandId     :: Int
    , commandMethod :: String
    , commandParams :: [(String, A.Value)]
    } deriving (Show)

instance ToJSON Command where
    toJSON cmd = A.object
        [ "id"     .= commandId cmd
        , "method" .= commandMethod cmd
        , "params" .= M.fromList (commandParams cmd)
        ]

-- Chrome Remote actions
goToPage :: String ->  Command
goToPage page = Command
    { commandId     = 1
    , commandMethod = "Page.navigate"
    , commandParams = [("url", toJSON page)]
    }

jsEval :: String -> Command
jsEval code = Command
    { commandId     = 1
    , commandMethod = "Runtime.evaluate"
    , commandParams =
        [ ("expression", toJSON code)
        ]
    }

reload :: Command
reload = Command
    { commandId     = 1
    , commandMethod = "Page.reload"
    , commandParams =
        [ ("ignoreCache", toJSON True)
        ]
    }

-- Linkedin specific
searchName :: String ->  Command
searchName name = jsEval (searchName "#main-search-box" name)
  where
    searchName :: String -> String -> String
    searchName inpt txt = concat ["$(",show inpt,").val(",show txt,");$(\"#global-search\").submit()"]
