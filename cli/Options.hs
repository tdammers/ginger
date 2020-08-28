module Options
where

import Data.Semigroup ( (<>) ) -- needed for GHC 8.0 and 8.2
import Options.Applicative


data TemplateSource
  = TemplateFromFile FilePath
  | TemplateFromStdin

data DataSource
  = DataFromFile FilePath
  | DataLiteral String
  | DataFromStdin

data Options
  = RunOptions TemplateSource DataSource

parseOptions :: [String] -> IO Options
parseOptions args =
  execParser $ info (options <**> helper)
    ( fullDesc
    <> header "ginger - A command-line interface for the Ginger template language"
    )

options :: Parser Options
options = runOptions

runOptions :: Parser Options
runOptions =
  RunOptions <$> templateSource <*> dataSource

templateSource :: Parser TemplateSource
templateSource =
  convert <$> option str
      ( long "template"
      <> short 't'
      <> metavar "TEMPLATE"
      <> help "Load ginger template from this file"
      <> value "-"
      )
  where
    convert "-" = TemplateFromStdin
    convert f = TemplateFromFile f

dataSource :: Parser DataSource
dataSource =
  dataFromFile <|> dataLiteral

dataFromFile :: Parser DataSource
dataFromFile =
  convert <$> option str
        ( long "data-file"
        <> metavar "DATAFILE"
        <> help "Load JSON or YAML data from this file (`-' to read from stdin)"
        )
  where
    convert "-" = DataFromStdin
    convert f = DataFromFile f

dataLiteral :: Parser DataSource
dataLiteral =
  DataLiteral <$> option str
        ( long "data"
        <> short 'd'
        <> metavar "DATA"
        <> help "Use specified (JSON or YAML) DATA"
        <> value "{}"
        )
