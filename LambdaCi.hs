{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty.Trans

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory
import System.Cmd
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader

data Project 
  =  Project { projName :: String,
               gitUrl :: String, 
               buildCmd :: String
             }

data LCIState 
  = LCIS { projects :: [Project],
           jobQueue :: TVar [Job],
           jobsDone :: TVar [Job] }

data Job 
  = Job { jobProj   :: Project,
          jobStatus :: TVar JobStatus,
          jobOutput :: TVar [String]   }        

data JobStatus = Pending | Running | Success | Failure

type LamCIM a = ScottyT TL.Text (ReaderT LCIState IO) a

main = lambdaCI []

lambdaCI projs = do 
   mapM_ initialise projs
   q <- atomically $ newTVar []
   done <- atomically $ newTVar []               
   scoot $ LCIS projs q done

--runReaderT :: ReaderT r m a -> r -> m a
runM st ra = runReaderT ra st

scoot st = scottyT 3001 (runM st) (runM st) routes

routes  :: LamCIM ()
routes =  do
  post "/github-webhook/:projname" $ do
    pNm <- param "projname"
    projs <- getProjects
    liftIO $ mapM_ build [ p | p <- projs, projName p == pNm]
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

initialise proj = do
  ensure_exists_or_pull proj

ensure_exists_or_pull proj = do
    ex <- doesDirectoryExist $ "/tmp/" ++ projName proj
    if ex 
       then void $ pull proj
       else void $ system $ "git clone " ++ gitUrl proj ++ " /tmp/"++projName proj

pull proj = do
  let nm = projName proj
  system $ "cd /tmp/" ++ nm ++ " && git pull"  

build proj = do
  pull proj 
  system $ "(cd /tmp/" ++ projName proj ++ " && "++buildCmd proj++")"  

getProjects = fmap projects $ lift ask