{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Stargaze.Manage
  ( loadConfig,
    writeConfig,
    updateLocalProjects,
    loadLocalProjects,
    showTopTags,
    showTopOwners,
    showTopLanguages,
    listProjects,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.ByteString.Lazy as B
import Data.Data (Proxy (Proxy))
import Data.HashMap (toList, (!))
import Data.List (isInfixOf, sortBy)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy as L
import Data.Time (getCurrentTime)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    QueryParam (queryParam),
    defaultHttpConfig,
    header,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
  )
import Path
  ( Abs,
    Dir,
    File,
    Path,
    mkRelDir,
    mkRelFile,
    toFilePath,
    (</>),
  )
import Path.IO
  ( createDir,
    doesDirExist,
    doesFileExist,
    getHomeDir,
  )
import Stargaze.Aggregate
  ( MergeMap (getMergeMap),
    ProjectAgg (aggAuthor, aggByLang, aggByOwner, aggByTag),
    aggregate,
  )
import Stargaze.Types (Author (authorLogin, authorHtmlUrl), Config (Config, cfgUpdatedAt, cfgUser), Project (projectHtmlUrl, projectLanguage, projectName, projectOwner, projectTopics), ProjectFilter (pfLanguage, pfPattern, pfTag, pfOwner))
import System.Exit (exitFailure, exitSuccess)
import Data.Char (toLower)
import Formatting
import Formatting.Time (dayHalf, dateDash, dayHalfU, hmsPL)

{- Maximum allowed page size -}
pageSize :: Int
pageSize = 100

loadConfig :: IO (Either Text Config)
loadConfig = do
  configFilePath <- getConfigFilePath
  existed <- doesFileExist configFilePath
  if not existed
    then do
      return $ Left "Config file doesn't exist, try \"stargaze config --user [GitHub Username]\""
    else do
      content <- B.readFile $ toFilePath configFilePath
      case decode content of
        Just cfg ->
          if cfgUser cfg /= ""
            then return $ Right cfg
            else do
              return $ Left "GitHub user name hasn't been config yet, try \"stargaze config --user [GitHub Username]\""
        _ -> do
          return $ Left $ T.append "Invalid config, please check your config at " (T.pack $ toFilePath configFilePath)

writeConfig :: Config -> IO ()
writeConfig cfg = do
  configFilePath <- getConfigFilePath
  let content = encodePretty cfg
  B.writeFile (toFilePath configFilePath) content

updateLocalProjects :: IO ()
updateLocalProjects = do
  config <- loadConfig
  case config of
    Left err -> T.putStrLn err
    Right config -> do
      if isJust (cfgUpdatedAt config)
        then do
          L.putStrLn $ format ("Last updated at " % dateDash <> " " % hmsPL) (fromJust $ cfgUpdatedAt config)
          loadAllProjects $ cfgUser config
        else loadAllProjects $ cfgUser config

loadAllProjects :: Text -> IO ()
loadAllProjects user = do
  projects <- loadAllProjects' user 1
  saveProjectsToLocal projects
  time <- getCurrentTime
  writeConfig $ Config user (Just time)
  T.putStrLn "Data load completed"

loadAllProjects' :: Text -> Int -> IO [Project]
loadAllProjects' user page = do
  projects <- loadPagedProjects user page
  if length projects < pageSize
    then do
      -- Last page
      L.putStrLn $ format ("Loaded " % int % " projects") (page * pageSize + length projects)
      return projects
    else do
      L.putStrLn $ format ("Loaded " % int % " projects") (page * pageSize)
      nextPageProjects <- loadAllProjects' user (page + 1)
      return $ projects ++ nextPageProjects

loadPagedProjects :: Text -> Int -> IO [Project]
loadPagedProjects user page = runReq defaultHttpConfig $ do
  let url = https "api.github.com" /: "users" /: user /: "starred"
  let options =
        queryParam "page" (Just $ show page)
          <> queryParam "per_page" (Just $ show pageSize)
          <> header "User-Agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.114 Safari/537.36 Edg/103.0.1264.49"
  response <- req GET url NoReqBody jsonResponse options
  return $ responseBody response

saveProjectsToLocal :: [Project] -> IO ()
saveProjectsToLocal projects = do
  projectsFilePath <- getProjectsFilePath
  let content = encodePretty projects
  B.writeFile (toFilePath projectsFilePath) content

loadLocalProjects :: IO (Either Text [Project])
loadLocalProjects = do
  projectsFilePath <- getProjectsFilePath
  existed <- doesFileExist projectsFilePath
  if not existed
    then return $ Left "Run \"stargaze update\" first"
    else do
      content <- B.readFile (toFilePath projectsFilePath)
      case decode content of
        Just projects -> return $ Right projects
        _ -> return $ Left "Run \"stargaze update\" first"

showTopTags :: Int -> [Project] -> IO ()
showTopTags n projects = do
  let agg = aggregate projects
  let tags = getMergeMap $ aggByTag agg
  let tagCounts = map (second length) (toList tags)
  let sortedTags = sortBy (\(_, a) (_, b) -> compare b a) tagCounts
  let headTags = take n sortedTags
  forM_ headTags $ \(tag, count) ->
    L.putStrLn $ format (text % " (" % int % ")") (L.fromStrict tag) count

showTopLanguages :: Int -> [Project] -> IO ()
showTopLanguages n projects = do
  let agg = aggregate projects
  let languages = getMergeMap $ aggByLang agg
  let langCounts = map (second length) (toList languages)
  let sortedLanguages = sortBy (\(_, a) (_, b) -> compare b a) langCounts
  let topLanguages = take n sortedLanguages
  forM_ topLanguages $ \(lang, count) ->
    L.putStrLn $ format (text % " (" % int % ")") (L.fromStrict lang) count

showTopOwners :: Int -> [Project] -> IO ()
showTopOwners n projects = do
  let agg = aggregate projects
  let authors = aggAuthor agg
  let owners = getMergeMap $ aggByOwner agg
  let ownerCounts = map (second length) (toList owners)
  let sortedOwners = sortBy (\(_, a) (_, b) -> compare b a) ownerCounts
  let headOwners = take n sortedOwners
  forM_ headOwners $ \(authorId, count) -> do
    let author = authors ! authorId
    L.putStrLn $ format ((right 20 ' ' %. (text % " (" % int % ")")) % " " % text)
                          (L.fromStrict $ authorLogin author)
                          count
                          (L.fromStrict $ authorHtmlUrl author)

listProjects :: ProjectFilter -> Int -> [Project] -> IO ()
listProjects pf n projects = do
  let projects' = filter matchFilter projects
  let headProjects = take n projects'
  forM_ headProjects $ \project -> do
    L.putStrLn $ format ((right 40 ' ' %. (text % "/" % text)) % " " % text)
                          (L.fromStrict $ authorLogin $ projectOwner project)
                          (L.fromStrict $ projectName project)
                          (L.fromStrict $ projectHtmlUrl project)
  where
    matchLanguage project = case pfLanguage pf of
      Just lang -> case projectLanguage project of
        Just lang' -> T.toLower lang ==  T.toLower lang'
        _ -> False
      _ -> True
    matchTopic project = case pfTag pf of
      Just tag ->  T.toLower tag `elem` map T.toLower (projectTopics project)
      _ -> True
    matchOnwer project = case pfOwner pf of
      Just owner ->  T.toLower owner `T.isInfixOf` ( T.toLower . authorLogin . projectOwner) project
      _ -> True
    matchPattern project = case pfPattern pf of
      Just pattern ->  T.toLower pattern `T.isInfixOf` ( T.toLower . projectName) project
      _ -> True
    matchFilter project = all (\match -> match project) [matchLanguage, matchTopic, matchOnwer, matchPattern]

getConfigFilePath :: IO (Path Abs File)
getConfigFilePath = do
  dotDir <- getOrCeateDotPath
  return $ dotDir </> $(mkRelFile "config.json")

getProjectsFilePath :: IO (Path Abs File)
getProjectsFilePath = do
  dotDir <- getOrCeateDotPath
  return $ dotDir </> $(mkRelFile "projects.json")

getOrCeateDotPath :: IO (Path Abs Dir)
getOrCeateDotPath = do
  homeDir <- getHomeDir
  let dotDir = homeDir </> $(mkRelDir ".stargaze")
  existed <- doesDirExist dotDir
  if existed
    then return dotDir
    else do
      createDir dotDir
      return dotDir
