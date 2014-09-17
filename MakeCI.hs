{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Web.Spock
import Web.Spock.Worker

import Control.Monad.Trans
import Control.Monad
import Data.Monoid
import Data.Time
import Data.List (isInfixOf)
import System.Directory
import System.Environment
import GHC.Generics
import Data.Aeson

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Database.Persist hiding (get)
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get)

import Utils
import Worker
import Views
import Database


type Route a = SpockM Connection SessionId () a
type Action a = SpockAction Connection SessionId () a


blaze :: H.Html -> Action ()
blaze = html . renderHtml



sessCfg = SessionCfg "makeci" (72*60*60) 42

main = do
   args <- getArgs
   case args of
     [] -> readProjects "/etc/makeci-repos" >>= makeCI
     file:_ -> readProjects file >>= makeCI


makeCI projects = do
    withSqlitePool "/tmp/makecidb" 5 $ \pool -> do
      spock 3001 sessCfg (PCPool pool) () (routes projects)


routes  :: [Project] -> Route ()
routes projects =  do
  runDB $ runMigration migrateAll
  runDB $ updateProjects projects

  worker <- newWorker 10 runBuild workErrH

  post "/github-webhook/:projname" $ do
    pNm <- param "projname"
    gh <- jsonData
    liftIO $ putStrLn $ "GitHub ref: "++ref gh
    when ("master" `isInfixOf` (ref gh)) $ do
       projs <- runDB $ selectList [ProjectRepoName ==. pNm] []
       mapM_ (startBuild worker) [ pid | Entity pid p <- projs]

  get "/" $ do
    projEnts <- runDB $ selectList [] []
    let projects = map projRow projEnts

    dbJobEnts <- runDB $ selectList [] [Desc JobId, LimitTo 100]

    let dbjobs = [jobRow (proj, Entity jid job ) |
                          Entity jid job <- dbJobEnts,
                          Entity pid proj <- projEnts,
                          pid == jobProject job ]

    let mreload = if all (done . jobStatus . entityVal) dbJobEnts
                     then mempty
                     else H.meta ! A.httpEquiv "refresh" ! A.content "2"

    blaze $ template "MakeCI" (mreload) $ do
            H.h1 "Projects"
            H.table ! A.class_ "table table-condensed" $ mconcat projects

            H.h1 "Jobs"
            H.table ! A.class_ "table table-condensed" $ mconcat (dbjobs)

  get "/build-now/:projid" $ do
    pid <- param "projid"
    startBuild worker pid
    redirect "/"

  get "/job/:jobid" $ do
    jobid <- param "jobid"
    mjob <- runDB $ P.get jobid
    case mjob of
      Nothing -> html $ "no job " <> tshow jobid
      Just job -> do Just proj <- runDB $ P.get (jobProject job)
                     blaze $ jobDisp proj job



updateProjects cfgProjs = do
   dbProjs <- selectList [] []
   let toDelete = [dbEnt | dbEnt <- dbProjs, not $ entityVal dbEnt `elem` cfgProjs]
   let toInsert = [proj | proj <- cfgProjs,  not $ proj `elem` map entityVal dbProjs]
   mapM_ (P.delete . entityKey) toDelete
   mapM_ insert toInsert
   liftIO $  mapM_ ensure_exists_or_pull cfgProjs
   updateWhere [JobStatus <-. ["Building", "Testing", "Pulling", "Pending"]] [JobStatus =. "BuildFailure"]



readProjects :: FilePath -> IO [Project]
readProjects file = do

  let f line = case span (/='/') line of
                 (_, []) -> []
                 (user, '/':repo) -> [Project user repo]

  ex <- doesFileExist file
  if not ex
     then help file >> fail "No configuration file"
     else fmap (concatMap f . lines) $ readFile file

help file = putStrLn $ unlines
  ["Cannot open configuration file "++file, "",
   "This file should have a {GitHub username}/{Repository name} on each line","",
   "for example:","",
   "glutamate/probably-base",
   "openbrainsrc/debcd",""]


startBuild worker projectId = do

    Just prj <- runDB $ P.get projectId

    now <- liftIO $ getCurrentTime

    jobId <- runDB $ insert $ Job projectId "" "" now Nothing "Pending" ("")

    addWork WorkNow jobId worker

workErrH errS jobId = do return WorkError

data GitHubPost = GitHubPost { ref :: String } deriving Generic

instance FromJSON GitHubPost
