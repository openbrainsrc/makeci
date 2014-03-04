{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import Control.Monad.Trans
import Control.Monad
import Data.Monoid
import Data.Time

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
import SpockWorker


type Route a = SpockM Connection SessionId (Queue JobId) a
type Action a = SpockAction Connection SessionId (Queue JobId) a


blaze :: H.Html -> Action ()
blaze = html . renderHtml



sessCfg = SessionCfg "makeci" (72*60*60) 42

main = readProjects "/etc/makeci-repos" >>= makeCI 


makeCI projs = do 
    withSqlitePool "/tmp/makecidb" 5 $ \pool -> do 
      runDB_io pool $ runMigration migrateAll
      runDB_io pool $ updateProjects projs
      workq <- spockWorker pool runBuild 
      spock 3001 sessCfg (PCPool pool) workq routes


routes  :: Route ()
routes =  do
  post "/github-webhook/:projname" $ do
    pNm <- param "projname"
    projs <- runDB $ selectList [ProjectRepoName ==. pNm] [] 
    mapM_ startBuild [ pid | Entity pid p <- projs]

  get "/" $ do
    projEnts <- runDB $ selectList [] []
    let projects = map projRow projEnts

    qJobIds <- readQueue =<< getState
    
    qjobs <- fmap (map (jobQRow . entityVal )) $ runDB $ selectList [JobId <-. qJobIds] [] 

    dbJobEnts <- runDB $ selectList [] [Desc JobId]
    let dbjobs = [jobRow (proj, Entity jid job ) |
                          Entity jid job <- dbJobEnts,
                          Entity pid proj <- projEnts,
                          pid == jobProject job ]

    let mreload = if null qjobs && all (done . jobStatus . entityVal) dbJobEnts
                     then mempty
                     else H.meta ! A.httpEquiv "refresh" ! A.content "2"

    blaze $ template "MakeCI" (mreload) $ do
            H.h1 "Projects"
            H.table ! A.class_ "table table-condensed" $ mconcat projects

            H.h1 "Jobs"
            H.table ! A.class_ "table table-condensed" $ mconcat (qjobs ++ dbjobs)

  get "/build-now/:projid" $ do
    pid <- param "projid"
    startBuild pid
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



readProjects :: FilePath -> IO [Project]
readProjects = fmap (concatMap f . lines) . readFile where
  f line = case span (/='/') line of 
             (_, []) -> []
             (user, '/':repo) -> [Project user repo]

startBuild projectId = do

    Just prj <- runDB $ P.get projectId

    pull prj

    now <- liftIO $ getCurrentTime
    gitres <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("git log --oneline -1")

    let (hash, commit) = case gitres of 
             Left err -> ("unknown", "unknown")
             Right s -> span (/=' ') s             
    jobId <- runDB $ insert $ Job projectId hash commit now Nothing "Building" ("")

    q <- getState
    addJob q jobId
