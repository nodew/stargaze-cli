module Stargaze.Command where
import Stargaze.Types (Config)
import Options.Applicative
    ( command,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strOption,
      subparser,
      helper,
      Parser,
      ParserInfo )

data Command = SetConfig { user :: String }
             | UpdateProjects
             | Search { pattern :: String }
             | ListOwners
             | ListTags
             deriving (Eq, Show)

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseUsername :: Parser String
parseUsername = strOption $ long "user" <> metavar "[GITHUB USERNAME]" <> help "Config your github username"

parseSearchPattern :: Parser String
parseSearchPattern = strOption $ long "pattern" <> short 'p' <> metavar "[PATTERN]" <> help "Search pattern"

parseSetConfig :: Parser Command
parseSetConfig = SetConfig <$> parseUsername

parseSearch :: Parser Command
parseSearch = Search <$> parseSearchPattern

parseCommand :: Parser Command
parseCommand = subparser
    ( command "config" (parseSetConfig `withInfo` "Config")
   <> command "update" (pure UpdateProjects `withInfo` "Update your project list from upstream")
   <> command "search" (parseSearch `withInfo` "Search for a project")
   <> command "list-owners" (pure ListOwners `withInfo` "List top owners")
   <> command "list-tags" (pure ListTags `withInfo` "List top tags")
   )
