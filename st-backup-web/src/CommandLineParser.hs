module CommandLineParser where

data CommandLineArgs = CommandLineArgs
  { _configFile :: FilePath
  }
  deriving (Show)

optionsParser :: Parser CommandLineArgs
optionsParser =
  CommandLineArgs
    <$> strOption (long "config-file" <> short 'c' <> metavar "CONFIG FILE")

parseCommandLine =
  execParser $
    info
      (optionsParser <**> helper)
      (fullDesc <> progDesc "ST Backup web server")
