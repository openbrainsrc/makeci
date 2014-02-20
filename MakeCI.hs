{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty.Trans

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import System.Process
import System.Cmd
import System.IO
import System.Exit
import Control.Concurrent.STM hiding (modifyTVar, atomically)

import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Control.Concurrent

import Data.Maybe
import Data.List

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (preEscapedText, preEscapedString)

import Types
import Utils
import Worker
import Views

main = makeCI   [Project "glutamate" "probably-base",
                 Project "glutamate" "matio",
                 Project "glutamate" "baysig-platform",
                 Project "ottigerb" "therapy-server",
                 Project "ottigerb" "ng-survey-server"]

makeCI projs = do 
   mapM_ ensure_exists_or_pull projs
   q <- atomically $ newTVar []
   done <- atomically $ newTVar []   
   ctr <- atomically $ newTVar 0
   print projs
   forkIO $ backworker q done
   scoot $ LCIS projs ctr q done

--runReaderT :: ReaderT r m a -> r -> m a
runM st ra = runReaderT ra st

scoot st = scottyT 3001 (runM st) (runM st) routes

routes  :: Route ()
routes =  do
  post "/github-webhook/:projname" $ do
    pNm <- param "projname"
    projs <- getProjects
    mapM_ build [ p | p <- projs, repoName p == pNm]

  get "/" $ do
    jobs <- getJobQueue >>= mapM  jobRow
    done_jobs <- getJobsDone >>= mapM  jobRow
    projects <- withState projects $ mapM projRow
    let mreload = if null jobs
                     then mempty
                     else H.meta ! A.httpEquiv "refresh" ! A.content "2"
    blaze $ template "MakeCI" (mreload) $ do
            H.h1 "Projects"
            H.table ! A.class_ "table table-condensed" $ mconcat projects

            H.h1 "Jobs (running)"
            H.table ! A.class_ "table table-condensed" $ mconcat jobs

            H.h1 "Jobs (done)"
            H.table ! A.class_ "table table-condensed" $ mconcat done_jobs

  get "/build-now/:projname" $ do
    pNm <- param "projname"
    projs <- getProjects
    mapM_ build [ p | p <- projs, repoName p == pNm]
    redirect "/"   

  get "/job/:jobid" $ do
    jobid <- param "jobid"
    jobs <- liftM2 (++) getJobQueue getJobsDone
    case [job | job <- jobs, jobId job == jobid] of
      [] -> html $ "no job " <> tshow jobid
      found:_  -> do jobdisp <-  jobDisp found
                     blaze jobdisp




