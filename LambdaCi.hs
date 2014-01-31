{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty.Trans

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory
import System.Cmd
import Control.Concurrent.STM hiding (modifyTVar, atomically)
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
data Project 
  =  Project { userName :: String,
               repoName :: String }
  deriving Show

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

instance Show Job where
  show (Job prj id _ _ ) = "Job ("++show prj++") "++show id

data JobStatus = Pending | Running | Success | Failure
  deriving Show
type LamCIM a = ScottyT TL.Text (ReaderT LCIState IO) a
type Action a = ActionT TL.Text (ReaderT LCIState IO) a

main = lambdaCI [Project "glutamate" "probably-base"]

lambdaCI projs = do 
   mapM_ initialise projs
   q <- STM.atomically $ newTVar []
   done <- STM.atomically $ newTVar []   
   ctr <- STM.atomically $ newTVar 0
   print projs
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

  get "/" $ do
    jobs <- getJobQueue >>= mapM jobRow
    projects <- withState projects $ mapM projRow
    html $ TL.concat ["<h1>Projects</h1><table>" <> TL.concat projects <> "</table>",
                      "<h1>Jobs</h1><table>" <> TL.concat jobs <> "</table>"]

  get "/build-now/:projname" $ do
    pNm <- param "projname"
    projs <- getProjects
    mapM_ build [ p | p <- projs, repoName p == pNm]
    redirect "/"   

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

jobRow (Job (Project u r) id statusTV outTV) = do
    status <- atomically $ readTVar statusTV
    out <- atomically $ readTVar outTV
    return $ TL.concat ["<tr><td>#", tshow id, 
                       "</td><td>", TL.pack u, 
                       "/", TL.pack r,"</td><td>",tshow status,"</td></tr>"]

projRow (Project u r) 
  = return $ TL.concat ["<tr><td>", TL.pack u, 
                        "/", TL.pack r,
                        "</td><td><a href=\"/build-now/", 
                        TL.pack r,
                        "\">Build now</a></td><tr>"]

tshow :: Show a => a -> TL.Text
tshow = TL.pack . show

initialise proj = do
  ensure_exists_or_pull proj

ensure_exists_or_pull proj = do
    ex <- doesDirectoryExist $ "/tmp/" ++ repoName proj
    if ex 
       then return () -- void $ pull proj
       else void $ system $ "git clone " ++ gitUrl proj ++ " /tmp/"++repoName proj

pull proj = do
  let nm = repoName proj
  
  liftIO $ system $ "cd /tmp/" ++ nm ++ " && git pull"  

build proj = do
  pull proj 
  jid <- freshId
  status <- atomically $ newTVar Pending
  out <- atomically $ newTVar []
  addJobToQueue $ Job proj jid status out
 -- liftIO $ system $ "(cd /tmp/" ++ projName proj ++ " && make)"  

getProjects = fmap projects $ lift ask

freshId :: Action Int
freshId  = do tv <- fmap counter $ lift ask
              modifyTVar tv (+1)

addJobToQueue :: Job -> Action ()
addJobToQueue jb = withState jobQueue $ \jqtv -> do
               void $ modifyTVar jqtv (jb:)
 

getJobQueue :: Action [Job]
getJobQueue = withState jobQueue $ atomically . readTVar 

printJobQueue :: Action ()
printJobQueue = do jq <- getJobQueue
                   liftIO $ putStrLn $ "JobQueue: "++ show jq

withState :: (LCIState -> a) -> (a -> Action b) -> Action b
withState getter m = fmap getter (lift ask) >>= m                        

modifyTVar :: TVar a -> (a -> a) -> Action a
modifyTVar tv f = atomically $ do 
     x <- readTVar tv
     let newx = f x
     writeTVar tv newx
     return newx

gitUrl (Project user repo) = "git@github.com:"++user++"/"++repo++".git"

atomically = liftIO . STM.atomically