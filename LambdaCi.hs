{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty.Trans

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory
import System.Cmd
import Control.Concurrent.STM hiding (modifyTVar)
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader

data Project 
  =  Project { userName :: String,
               repoName :: String }

data LCIState 
  = LCIS { projects :: [Project],
           counter :: TVar Int,
           jobQueue :: TVar [Job],
           jobsDone :: TVar [Job] }

data Job 
  = Job { jobProj   :: Project,
          jobId     :: Int,
          jobStatus :: TVar JobStatus,
          jobOutput :: TVar [String]   }        

data JobStatus = Pending | Running | Success | Failure

type LamCIM a = ScottyT TL.Text (ReaderT LCIState IO) a
type Action a = ActionT TL.Text (ReaderT LCIState IO) a

main = lambdaCI []

lambdaCI projs = do 
   mapM_ initialise projs
   q <- atomically $ newTVar []
   done <- atomically $ newTVar []   
   ctr <- atomically $ newTVar 0
   scoot $ LCIS projs ctr q done

--runReaderT :: ReaderT r m a -> r -> m a
runM st ra = runReaderT ra st

scoot st = scottyT 3001 (runM st) (runM st) routes

routes  :: LamCIM ()
routes =  do
  post "/github-webhook/:projname" $ do
    pNm <- param "projname"
    projs <- getProjects
    mapM_ build [ p | p <- projs, repoName p == pNm]
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

initialise proj = do
  ensure_exists_or_pull proj

ensure_exists_or_pull proj = do
    ex <- doesDirectoryExist $ "/tmp/" ++ repoName proj
    if ex 
       then void $ pull proj
       else void $ system $ "git clone " ++ gitUrl proj ++ " /tmp/"++repoName proj

pull proj = do
  let nm = repoName proj
  
  liftIO $ system $ "cd /tmp/" ++ nm ++ " && git pull"  

build proj = do
  pull proj 
  jid <- freshId
  status <- liftIO $ atomically $ newTVar Pending
  out <- liftIO $ atomically $ newTVar []
  addJobToQueue $ Job proj jid status out
 -- liftIO $ system $ "(cd /tmp/" ++ projName proj ++ " && make)"  

getProjects = fmap projects $ lift ask

freshId :: Action Int
freshId  = do tv <- fmap counter $ lift ask
              modifyTVar tv (+1)

addJobToQueue :: Job -> Action ()
addJobToQueue jb = do 
               tv <- fmap jobQueue $ lift ask
               void $ modifyTVar tv (jb:)
 

modifyTVar :: TVar a -> (a -> a) -> Action a
modifyTVar tv f = liftIO $ atomically $ do 
     x <- readTVar tv
     let newx = f x
     writeTVar tv newx
     return newx

gitUrl (Project user repo) = "git@github.com:"++user++"/"++repo++".git"