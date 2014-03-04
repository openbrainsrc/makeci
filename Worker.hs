{-# LANGUAGE OverloadedStrings #-}

module Worker where

import Utils

import qualified Control.Concurrent.STM as STM
import Data.Maybe
import Data.List (stripPrefix)
import Text.Blaze.Internal (preEscapedText, preEscapedString)
import Control.Monad
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Cmd
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Trans
import System.Directory
import Data.Time

import Database
import           Database.Persist

import Web.Spock (getState)

import SpockWorker

gitUrl (Project user repo) = "git@github.com:"++user++"/"++repo++".git"

ensure_exists_or_pull proj = do
    ex <- doesDirectoryExist $ "/tmp/" ++ projectRepoName proj
    if ex 
       then return () -- void $ pull proj
       else void $ system $ "git clone " ++ gitUrl proj ++ " /tmp/"++projectRepoName proj

pull proj = do
  let nm = projectRepoName proj
  
  liftIO $ system $ "cd /tmp/" ++ nm ++ " && git pull"  


--runBuild :: ProjectId -> WorkerM conn ()
runBuild projectId =  do
    Just prj <- runDBw $ get projectId

    pull prj

    now <- liftIO $ getCurrentTime
    gitres <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("git log --oneline -1")

    let (hash, commit) = case gitres of 
             Left err -> ("unknown", "unknown")
             Right s -> span (/=' ') s             
  
    -- create job 
    jobId <- runDBw $ insert $ Job projectId hash commit now Nothing "Building" ("")

    let updateJ = runDBw . update jobId

    res <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("make cibuild")
    case res of
      Left errS -> updateJ [JobOutput =. pre errS,
                            JobStatus =. "BuildFailure"]
      Right resS -> do updateJ [JobOutput =. pre resS,
                                JobStatus =. "Testing"]  
                       res <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("make citest")
                       case res of
                         Left terrS -> do
                            updateJ [JobOutput =. (pre resS >> pre terrS),
                                     JobStatus =. "TestFailure"]  
                         Right sresS -> do
                            extraOutput <- getExtraOutput sresS
                            now <- liftIO $ getCurrentTime
                            updateJ [JobOutput =. (pre resS >> pre sresS >> extraOutput),
                                     JobStatus =. "Success",
                                     JobFinished =. Just now]

getExtraOutput sresS = liftIO $ do
  let files = catMaybes $ map (stripPrefix "file://") $ lines sresS
  fmap (preEscapedText . mconcat) $ mapM T.readFile files

pre = H.pre . preEscapedString