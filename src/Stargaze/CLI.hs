{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Stargaze.CLI
( loadConfig
, writeConfig
, updateLocalProjects
, loadLocalProjects
, showTopTags
, showTopOwners
, showTopLanguages
, listProjects
) where

import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      header,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody),
      QueryParam(queryParam) )
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Data (Proxy(Proxy))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad as T
import Path
    ( Path, Dir, Abs, File, toFilePath, (</>), mkRelDir, mkRelFile )
import Path.IO
    ( createDir, doesDirExist, doesFileExist, getHomeDir )
import System.Exit (exitFailure, exitSuccess)
import Data.Time (getCurrentTime)
import Data.Maybe (isJust)
import Data.List ( sortBy, isInfixOf )
import Data.HashMap (toList, (!))
import Data.Bifunctor ( Bifunctor(second) )
import Control.Monad (forM_)

import Stargaze.Types ( Project (projectLanguage, projectTopics, projectName, projectOwner, projectHtmlUrl), Config(cfgUser, Config, cfgUpdatedAt), Author (authorLogin), ProjectFilter (pfLanguage, pfTag, pfPattern) )
import Stargaze.Aggregate
    ( aggregate,
      MergeMap(getMergeMap),
      ProjectAgg(aggByTag, aggAuthor, aggByOwner, aggByLang) )

{- Maximum allowed page size -}
pageSize :: Int
pageSize = 100

loadConfig :: IO (Either String Config)
loadConfig = do
    configFilePath <- getConfigFilePath
    existed <- doesFileExist configFilePath
    if not existed then do
        return $ Left "Config file doesn't exist, try \"stargaze config --user [GitHub Username]\""
    else do
        content <- B.readFile $ toFilePath configFilePath
        case decode content of
            Just cfg ->
                if cfgUser cfg /= "" then
                    return $ Right cfg
                else do
                    return $ Left "GitHub user name hasn't been config yet, try \"stargaze config --user [GitHub Username]\""
            _ -> do
                return $ Left $ "Invalid config, please check your config at " ++ toFilePath configFilePath

writeConfig :: Config -> IO ()
writeConfig cfg = do
    configFilePath <- getConfigFilePath
    let content = encodePretty cfg
    B.writeFile (toFilePath configFilePath) content

updateLocalProjects :: IO ()
updateLocalProjects = do
    config <- loadConfig
    case config of
        Left err -> putStrLn err
        Right config -> do
            if isJust (cfgUpdatedAt config) then do
                putStrLn $ "Last updated at " ++ show (cfgUpdatedAt config)
                loadAllProjects $ cfgUser config
            else
                loadAllProjects $ cfgUser config

loadAllProjects :: String -> IO ()
loadAllProjects user = do
    projects <- loadAllProjects' user 1
    saveProjectsToLocal projects
    time <- getCurrentTime
    writeConfig $ Config user (Just time)
    putStrLn "Data load completed"

loadAllProjects' :: String -> Int -> IO [Project]
loadAllProjects' user page = do
    projects <- loadPagedProjects user page
    if length projects < pageSize then do -- Last page
        putStrLn $ "Loaded " ++ show (page * pageSize + length projects) ++ " projects"
        return projects
    else do
        putStrLn $ "Loaded " ++ show (page * pageSize) ++ " projects"
        nextPageProjects <- loadAllProjects' user (page + 1)
        return $ projects ++ nextPageProjects

loadPagedProjects :: String -> Int -> IO [Project]
loadPagedProjects user page = runReq defaultHttpConfig $ do
    let url = https "api.github.com" /: "users" /: T.pack user /: "starred"
    let options = queryParam "page" (Just $ show page)
               <> queryParam "per_page" (Just $ show pageSize)
               <> header "User-Agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.114 Safari/537.36 Edg/103.0.1264.49"
    response <- req GET url NoReqBody jsonResponse options
    return $ responseBody response

saveProjectsToLocal :: [Project] -> IO ()
saveProjectsToLocal projects = do
    projectsFilePath <- getProjectsFilePath
    let content = encodePretty projects
    B.writeFile (toFilePath projectsFilePath) content

loadLocalProjects :: IO (Either String [Project])
loadLocalProjects = do
    projectsFilePath <- getProjectsFilePath
    existed <- doesFileExist projectsFilePath
    if not existed then
        return $ Left "Run \"stargaze update\" first"
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
    let sortedTags = sortBy (\ (_, a) (_, b) -> compare b a) tagCounts
    let headTags = take n sortedTags
    forM_ headTags $ \(tag, count) -> putStrLn $ tag ++ " (" ++ show count ++ ")"

showTopLanguages :: Int -> [Project] -> IO ()
showTopLanguages n projects = do
    let agg = aggregate projects
    let languages = getMergeMap $ aggByLang agg
    let langCounts = map (second length) (toList languages)
    let sortedLanguages = sortBy (\ (_, a) (_, b) -> compare b a) langCounts
    let topLanguages = take n sortedLanguages
    forM_ topLanguages $ \(lang, count) -> putStrLn $ lang ++ " (" ++ show count ++ ")"

showTopOwners :: Int -> [Project] -> IO ()
showTopOwners n projects = do
    let agg = aggregate projects
    let authors = aggAuthor agg
    let owners = getMergeMap $ aggByOwner agg
    let ownerCounts = map (second length) (toList owners)
    let sortedOwners = sortBy (\ (_, a) (_, b) -> compare b a) ownerCounts
    let headOwners = take n sortedOwners
    forM_ headOwners $ \(authorId, count) -> do
        let author = authors ! authorId
        putStrLn $ authorLogin author ++ " (" ++ show count ++ ")"

listProjects :: ProjectFilter -> Int -> [Project] -> IO ()
listProjects pf n projects = do
    let projects' = filter matchFilter projects
    let headProjects = take n projects'
    forM_ headProjects $ \project -> do
        putStrLn $ (authorLogin . projectOwner) project ++ "/" ++ projectName project ++ " " ++ projectHtmlUrl project
    where
        matchLanguage project = case pfLanguage pf of
            Just lang -> case projectLanguage project of
                            Just lang' -> lang == lang'
                            _          -> False
            _         -> True
        matchTopic project = case pfTag pf of
            Just tag -> tag `elem` projectTopics project
            _         -> True
        matchPattern project = case pfPattern pf of
            Just pattern -> pattern `isInfixOf` projectName project
            _         -> True
        matchFilter project = matchLanguage project && matchTopic project && matchPattern project

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
    if existed then
        return dotDir
    else do
        createDir dotDir
        return dotDir
