{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
  ( customExecParser,
    helper,
    idm,
    info,
    prefs,
    showHelpOnEmpty,
    (<**>),
  )
import Stargaze.Manage
  ( listProjects,
    loadConfig,
    loadLocalProjects,
    showTopLanguages,
    showTopOwners,
    showTopTags,
    updateLocalProjects,
    writeConfig,
  )
import Stargaze.Command
  ( Command (ListLang, ListOwners, ListProject, ListTags, SetConfig, UpdateProjects),
    parseCommand,
  )
import Stargaze.Types (Config (Config, cfgUser))

main :: IO ()
main = do
  options <- customExecParser p opts
  exec options
  where
    opts = info (parseCommand <**> helper) idm
    p = prefs showHelpOnEmpty

exec :: Command -> IO ()
exec (SetConfig user) =
  loadConfig >>= \case
    Left err -> writeConfig $ Config user Nothing
    Right config -> writeConfig $ config {cfgUser = user}
exec UpdateProjects = updateLocalProjects
exec ListTags =
  loadLocalProjects >>= \case
    Left err -> putStrLn err
    Right projects -> showTopTags 20 projects
exec ListOwners =
  loadLocalProjects >>= \case
    Left err -> putStrLn err
    Right projects -> showTopOwners 20 projects
exec ListLang =
  loadLocalProjects >>= \case
    Left err -> putStrLn err
    Right projects -> showTopLanguages 20 projects
exec (ListProject pf) =
  loadLocalProjects >>= \case
    Left err -> putStrLn err
    Right projects -> listProjects pf 20 projects
