{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty.Trans

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory
import System.Process
import System.Cmd
import System.IO
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM hiding (modifyTVar, atomically)

import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Reader
import Data.Monoid

import qualified Control.Exception as C
import Control.Exception
import Control.DeepSeq (rnf)


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

data JobStatus = Pending | Pulling | Running | Success | Failure
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
   forkIO $ backworker q done
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
    done_jobs <- getJobsDone >>= mapM jobRow
    projects <- withState projects $ mapM projRow
    html $ TL.concat ["<h1>Projects</h1><table>" <> TL.concat projects <> "</table>",
                      "<h1>Jobs (running)</h1><table>" <> TL.concat jobs <> "</table>",
                      "<h1>Jobs (done)</h1><table>" <> TL.concat done_jobs <> "</table>"]

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
      founds  -> do jobdisps <- mapM jobDisp founds
                    html $  TL.concat jobdisps 

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

jobRow (Job (Project u r) id statusTV outTV) = do
    status <- atomically $ readTVar statusTV
    return $ TL.concat ["<tr><td>#", tshow id, 
                       "</td><td>", TL.pack u, 
                       "/", TL.pack r,"</td><td><a href=\"/job/",
                       tshow id,"\">",tshow status,"</a></td></tr>"]

jobDisp job = do
  row <- jobRow job
  outSS <- atomically $ readTVar $ jobOutput job
  let outT = TL.unlines $ reverse $ map TL.pack outSS
  return $ TL.concat ["<table>", row, "</table><pre>", outT,"</pre>"]

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
  jid <- freshId
  status <- atomically $ newTVar Pending
  out <- atomically $ newTVar []
  let job = Job proj jid status out
  addJobToQueue job
--  liftIO $ runBuild job
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

getJobsDone :: Action [Job]
getJobsDone = withState jobsDone $ atomically . readTVar 

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


addJobOutput :: Job -> String -> IO ()
addJobOutput (Job prj jid statusTV outTV) s = STM.atomically $ do
  old_out <- readTVar outTV
  writeTVar outTV (s:old_out)
  


backworker :: TVar [Job] -> TVar [Job] -> IO ()
backworker q done = do
   job <- STM.atomically $ do 
             jobs <- readTVar q
             case jobs of 
               [] -> retry
               job:more_jobs -> return job
   runBuild job
   STM.atomically $ STM.modifyTVar q (filter ((/=(jobId job)) . jobId))
   STM.atomically $ STM.modifyTVar done (job:)
   backworker q done
                                
runBuild job@(Job prj jid statusTV outTV) =  do 
    STM.atomically $ writeTVar statusTV Pulling
    pull prj
    STM.atomically $ writeTVar statusTV Running
    res <- psh ("/tmp/"++repoName prj) ("make cibuild")
    print res
    case res of
      Left errS -> STM.atomically $ do
                      writeTVar outTV [errS]
                      writeTVar statusTV Failure
      Right resS -> STM.atomically $ do
                      writeTVar outTV [resS]
                      writeTVar statusTV Success


-- from Baysig.Utils, by Ian Ross

psh :: String -> String -> IO (Either String String)
psh pwd cmd =
  bracketOnError
  (createProcess $ (shell cmd) { std_out = CreatePipe
                               , std_err = CreatePipe
                               , create_group = True 
                               , cwd = Just pwd })
  (\(_, Just hout, Just herr, ph) -> do
      interruptProcessGroupOf ph
      terminateProcess ph
      _ <- slurp hout herr
      _ <- waitForProcess ph
      return $ Left "Terminated")
  (\(_, Just hout, Just herr, ph) -> do
      (sout, serr) <- slurp hout herr
      excode <- waitForProcess ph
      case excode of
        ExitSuccess -> return $ Right sout
        ExitFailure _ -> return $ Left serr)
  where slurp hout herr = do
          sout <- hGetContents hout ; serr <- hGetContents herr
          waitOut <- forkWait sout  ; waitErr <- forkWait serr
          waitOut                   ; waitErr
          hClose hout               ; hClose herr
          return (sout, serr)
        forkWait a = do
          res <- newEmptyMVar
          _ <- mask $ \restore ->
            forkIO $ try (restore $ C.evaluate $ rnf a) >>= putMVar res
          return (takeMVar res >>=
                  either (\ex -> throwIO (ex :: SomeException)) return)