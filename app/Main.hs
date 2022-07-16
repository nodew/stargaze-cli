{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
    ( (<**>),
      idm,
      info,
      prefs,
      showHelpOnEmpty,
      customExecParser,
      helper )

import Stargaze.Types ( Config(Config, cfgUser) )
import Stargaze.Command
    ( parseCommand,
      Command(ListOwners, SetConfig, UpdateProjects, ListTags) )
import Stargaze.CLI
    ( loadConfig,
      loadLocalProjects,
      showTopOwners,
      showTopTags,
      updateLocalProjects,
      writeConfig )

main :: IO ()
main = do
    options <- customExecParser p opts
    exec options
    where
        opts = info (parseCommand <**> helper) idm
        p = prefs showHelpOnEmpty

exec :: Command -> IO ()

exec (SetConfig user) = loadConfig >>= \case
    Left err -> writeConfig $ Config user Nothing
    Right config -> writeConfig $ config { cfgUser = user }

exec UpdateProjects = updateLocalProjects

exec ListTags = loadLocalProjects >>= \case
    Left err -> putStrLn err
    Right projects -> showTopTags 20 projects

exec ListOwners = loadLocalProjects >>= \case
    Left err -> putStrLn err
    Right projects -> showTopOwners 20 projects

exec _ = putStrLn "Not implement yet"
