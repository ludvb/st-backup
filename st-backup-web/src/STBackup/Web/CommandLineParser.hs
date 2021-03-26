module STBackup.Web.CommandLineParser where

import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
    (<**>),
  )

data CommandLineArgs = CommandLineArgs
  { _configFile :: FilePath
  }
  deriving (Show)

optionsParser :: Parser CommandLineArgs
optionsParser = do
  CommandLineArgs
    <$> strOption (long "config-file" <> short 'c' <> metavar "CONFIG FILE")

parseCommandLine :: IO CommandLineArgs
parseCommandLine =
  execParser $
    info
      (optionsParser <**> helper)
      (fullDesc <> progDesc "ST Backup API server")
