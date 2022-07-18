module Main where

import Options.Applicative
    ( showHelpOnEmpty,
      prefs,
      idm,
      helper,
      (<**>),
      info,
      customExecParser )
import Stargaze.Command ( execCommand, parseCommand )

main :: IO ()
main = do
  options <- customExecParser p opts
  execCommand options
  where
    opts = info (parseCommand <**> helper) idm
    p = prefs showHelpOnEmpty
