{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import System.Process
import System.Cmd
import System.IO
import System.Exit
import Control.Concurrent.STM 
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Control.Concurrent

import Data.Maybe
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText, preEscapedString)

import Utils
import Worker
import Views
import Database
import SpockWorker

import           Database.Persist hiding (get)
import           qualified Database.Persist as P 
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

type Route a = SpockM Connection SessionId (Queue ProjectId) a
type Action a = SpockAction Connection SessionId (Queue ProjectId) a


blaze :: H.Html -> Action ()
blaze = html . renderHtml



sessCfg = SessionCfg "makeci" (72*60*60) 42

main = readProjects "/etc/makeci-repos" >>= makeCI 

             {- [Project "glutamate" "probably-base",
                 Project "glutamate" "matio",
                 Project "glutamate" "baysig-platform",
                 Project "ottigerb" "therapy-server",
                 Project "ottigerb" "ng-survey-server"] -}

readProjects :: FilePath -> IO [Project]
readProjects = fmap (concatMap f . lines) . readFile where
  f line = case span (/='/') line of 
             (_, []) -> []
             (user, '/':repo) -> [Project user repo]

makeCI projs = do 
   mapM_ ensure_exists_or_pull projs
   
   jobqueue <- atomically $ newTVar []
   withSqlitePool ":memory:" 5 $ \pool -> do 
      runDB_io pool $ runMigration migrateAll
      runDB_io pool $ mapM_ insert projs
      workq <- spockWorker pool runBuild 
      spock 2999 sessCfg (PCPool pool) workq routes


routes  :: Route ()
routes =  do
  post "/github-webhook/:projname" $ do
    pNm <- param "projname"
    projs <- runDB $ selectList [ProjectRepoName ==. pNm] [] 
    q <- getState
    mapM_ (addJob q) [ pid | Entity pid p <- projs]

  get "/" $ do
    projEnts <- runDB $ selectList [] []
    let projects = map projRow projEnts

    qProjIds <- readQueue =<< getState
    jobs <- fmap (map (jobQRow . entityVal )) $ runDB $ selectList [ProjectId <-. qProjIds] [] 

    doneJobEnts <- runDB $ selectList [] []
    let done_jobs = [jobRow (proj, Entity jid job ) |
                          Entity jid job <- doneJobEnts,
                          Entity pid proj <- projEnts,
                          pid == jobProject job ]

--getJobsDone >>= mapM  jobRow

    let mreload = if null jobs
                     then mempty
                     else H.meta ! A.httpEquiv "refresh" ! A.content "2"
    blaze $ template "MakeCI" (mreload) $ do
            H.h1 "Projects"
            H.table ! A.class_ "table table-condensed" $ mconcat projects

            H.h1 "Jobs"
            H.table ! A.class_ "table table-condensed" $ mconcat (jobs ++ done_jobs)

  get "/build-now/:projid" $ do
    pid <- param "projid"
    q <- getState
    addJob q pid 
    redirect "/"   

  get "/job/:jobid" $ do
    jobid <- param "jobid"
    mjob <- runDB $ P.get jobid
    case mjob of
      Nothing -> html $ "no job " <> tshow jobid
      Just job -> do Just proj <- runDB $ P.get (jobProject job)
                     blaze $ jobDisp proj job




