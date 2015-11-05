module CmdArgs where

import           Data.List.Split     (splitOn)
import           Options.Applicative

-- | Command line arguments for yesod devel.
-- All arguments are optional.
data CmdArgs = CmdArgs
  { restartChrome :: Bool
  , website :: FilePath
  , files :: [String]
  } deriving (Show, Eq)

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
  <$> ((=="true") <$> strOption (long "restartChrome" <> short 'r' <> value "false"))
  <*>  strOption
    ( long "path"
      <> short 'p'
      -- <> value "Application.hs"
      <> metavar "FILEPATH"
      <> help "The file with the function you want to run. Default is `Application.hs`.")
  <*>  (splitOn ","
    <$> strOption
      ( long "watch-directories"
        <> short 'w'
        <> value []
        <> metavar "DIRECTORY-LIST"
        <> help "A comma-separated list of directories to watch for any changes (not just .hs files)."))