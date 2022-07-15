{-# LANGUAGE OverloadedStrings  #-}

module CLI
    ( loadProjects
    ) where

import Network.HTTP.Req
import Data.Data (Proxy(Proxy))
import qualified Data.Text as T
import Types
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad as T

{- Maximum allowed page size -}
pageSize = 100

loadProjects = do
    projects <- loadPagedProjects "nodew" 1
    print $ length projects
    print projects

loadPagedProjects :: String -> Int -> IO [Project]
loadPagedProjects user page = runReq defaultHttpConfig $ do
    let url = https "api.github.com" /: "users" /: T.pack user /: "starred"
    let options = queryParam "page" (Just $ show page)
               <> queryParam "per_page" (Just $ show pageSize)
               <> header "User-Agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.114 Safari/537.36 Edg/103.0.1264.49"
    liftIO $ print url
    response <- req GET url NoReqBody jsonResponse options
    return $ responseBody response
