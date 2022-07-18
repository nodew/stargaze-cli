{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Stargaze.Command where

import Data.Text (Text)
import qualified Data.Text.IO as T
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

import Stargaze.Types (Config (cfgUser, Config), ProjectFilter (ProjectFilter))
import Stargaze.Manage
    ( loadConfig,
      writeConfig,
      updateLocalProjects,
      loadLocalProjects,
      showTopTags,
      showTopLanguages,
      showTopOwners,
      listProjects )

data Command
  = SetConfig {user :: Text}
  | UpdateProjects
  | ListProjects ProjectFilter Int
  | ListOwners Int
  | ListTags Int
  | ListLang Int
  deriving (Eq, Show)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseUsername :: Parser Text
parseUsername = strOption $ long "user" <> metavar "[GITHUB USERNAME]" <> help "Config your github username"

parseSearchPattern :: Parser Text
parseSearchPattern =
  strOption $
    long "pattern"
      <> short 'p'
      <> metavar "[PATTERN]"
      <> help "Search pattern"

parseLangOption :: Parser Text
parseLangOption =
  strOption $
    long "lang"
      <> short 'l'
      <> metavar "[LANGUAGE]"
      <> help "Filter by language"

parseTagOption :: Parser Text
parseTagOption =
  strOption $
    long "tag"
      <> short 't'
      <> metavar "[TAG]"
      <> help "Filter by tag"

parseOwnerOption :: Parser Text
parseOwnerOption =
  strOption $
    long "owner"
      <> short 'o'
      <> metavar "[OWNER]"
      <> help "Filter by owner"

parseTopOption :: Parser Int
parseTopOption = option auto $
    long "top"
      <> metavar "[N = 20]"
      <> value 20
      <> help "Return top N results"

strToMaybe :: Text -> Maybe Text
strToMaybe "" = Nothing
strToMaybe x = Just x

parseSetConfig :: Parser Command
parseSetConfig = SetConfig <$> parseUsername

parseListProjects :: Parser Command
parseListProjects =
  ListProjects <$>
    (ProjectFilter
      <$> optional parseSearchPattern
      <*> optional parseLangOption
      <*> optional parseTagOption
      <*> optional parseOwnerOption)
    <*> parseTopOption

parseListTags :: Parser Command
parseListTags = ListTags <$> parseTopOption

parseListLang :: Parser Command
parseListLang = ListLang <$> parseTopOption

parseListOwners :: Parser Command
parseListOwners = ListOwners <$> parseTopOption

parseCommand :: Parser Command
parseCommand =
  subparser
    ( command "config" (parseSetConfig `withInfo` "Config")
        <> command "update" (pure UpdateProjects `withInfo` "Update your project list from upstream")
        <> command "list" (parseListProjects `withInfo` "List projects")
        <> command "owners" (parseListOwners `withInfo` "List top owners")
        <> command "tags" (parseListTags `withInfo` "List top tags")
        <> command "languages" (parseListLang `withInfo` "List top languages")
    )

execCommand :: Command -> IO ()
execCommand (SetConfig user) =
  loadConfig >>= \case
    Left err -> writeConfig $ Config user Nothing
    Right config -> writeConfig $ config {cfgUser = user}
execCommand UpdateProjects = updateLocalProjects
execCommand (ListTags n) =
  loadLocalProjects >>= \case
    Left err -> T.putStrLn err
    Right projects -> showTopTags n projects
execCommand (ListOwners n) =
  loadLocalProjects >>= \case
    Left err -> T.putStrLn err
    Right projects -> showTopOwners n projects
execCommand (ListLang n) =
  loadLocalProjects >>= \case
    Left err -> T.putStrLn err
    Right projects -> showTopLanguages n projects
execCommand (ListProjects pf n) =
  loadLocalProjects >>= \case
    Left err -> T.putStrLn err
    Right projects -> listProjects pf n projects
