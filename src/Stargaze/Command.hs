module Stargaze.Command where

import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    command,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    progDesc,
    short,
    strOption,
    subparser,
    value,
  )
import Stargaze.Types (Config, ProjectFilter (ProjectFilter))

data Command
  = SetConfig {user :: String}
  | UpdateProjects
  | ListProject ProjectFilter
  | ListOwners
  | ListTags
  | ListLang
  deriving (Eq, Show)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseUsername :: Parser String
parseUsername = strOption $ long "user" <> metavar "[GITHUB USERNAME]" <> help "Config your github username"

parseSearchPattern :: Parser String
parseSearchPattern =
  strOption $
    long "pattern"
      <> short 'p'
      <> metavar "[PATTERN]"
      <> help "Search pattern"

parseSearchLang :: Parser String
parseSearchLang =
  strOption $
    long "lang"
      <> short 'l'
      <> metavar "[LANGUAGE]"
      <> help "Filter by language"

parseSearchTag :: Parser String
parseSearchTag =
  strOption $
    long "tag"
      <> short 't'
      <> metavar "[TAG]"
      <> help "Filter by tag"

strToMaybe :: String -> Maybe String
strToMaybe "" = Nothing
strToMaybe x = Just x

parseSetConfig :: Parser Command
parseSetConfig = SetConfig <$> parseUsername

parseSearch :: Parser Command
parseSearch =
  fmap ListProject $
    ProjectFilter
      <$> optional parseSearchPattern
      <*> optional parseSearchLang
      <*> optional parseSearchTag

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "config" (parseSetConfig `withInfo` "Config")
        <> command "update" (pure UpdateProjects `withInfo` "Update your project list from upstream")
        <> command "list" (parseSearch `withInfo` "Search project")
        <> command "list-owners" (pure ListOwners `withInfo` "List top owners")
        <> command "list-tags" (pure ListTags `withInfo` "List top tags")
        <> command "list-lang" (pure ListLang `withInfo` "List top languages")
    )
