{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Stargaze.Types where

import Data.Time ( UTCTime )
import Data.Int (Int64, Int32)
import Control.Monad ( MonadPlus(mzero) )
import GHC.Generics (Generic)
import Data.Aeson.Types (camelTo)
import Data.Aeson
    ( (.:),
      genericParseJSON,
      camelTo2,
      defaultOptions,
      object,
      FromJSON(parseJSON),
      Options(fieldLabelModifier),
      Value(Object),
      KeyValue((.=)),
      ToJSON(toJSON), genericToJSON )

data Config = Config
    { cfgUser      :: String
    , cfgUpdatedAt :: Maybe UTCTime
    } deriving (Show)

instance FromJSON Config where
    parseJSON (Object o) =
        Config <$> o .: "user"
               <*> o .: "updated_at"
    parseJSON _ = mzero

instance ToJSON Config where
    toJSON cfg = object
        [ "user"       .= toJSON (cfgUser cfg)
        , "updated_at" .= toJSON (cfgUpdatedAt cfg)
        ]

data Project = Project
    { projectId               :: Int64
    , projectNodeId           :: String
    , projectName             :: String
    , projectFullName         :: String
    , projectDescription      :: Maybe String
    , projectOwner            :: Author
    , projectHtmlUrl          :: String
    , projectSshUrl           :: String
    , projectCloneUrl         :: String
    , projectTopics           :: [String]
    , projectForksCount       :: Int32
    , projectWatchersCount    :: Int32
    , projectStargazersCount  :: Int32
    } deriving (Show, Eq, Generic)

instance FromJSON Project where
    parseJSON = genericParseJSON
        defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 7 }

instance ToJSON Project where
    toJSON = genericToJSON
        defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 7 }

data Author = Author
    { authorLogin     :: String
    , authorId        :: Int64
    , authorNodeId    :: String
    , authorAvatarUrl :: String
    , authorHtmlUrl   :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Author where
    parseJSON = genericParseJSON
        defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance ToJSON Author where
    toJSON = genericToJSON
        defaultOptions
            { fieldLabelModifier = camelTo2 '_' . drop 6 }