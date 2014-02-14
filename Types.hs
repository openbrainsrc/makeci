{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Types where

import Web.Scotty.Trans
import qualified Text.Blaze.Html5 as H
import Control.Monad.Trans
import Control.Monad
import qualified Data.Text.Lazy as TL
import Control.Monad.Reader
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Control.Concurrent.STM hiding (modifyTVar, atomically)
import Utils



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
          jobOutput :: TVar H.Html   }       

instance Show Job where
  show (Job prj id _ _ ) = "Job ("++show prj++") "++show id

data JobStatus = Pending | Pulling | Building | Testing | Success | BuildFailure | TestFailure
  deriving Show

type Route a = ScottyT TL.Text (ReaderT LCIState IO) a
type Action a = ActionT TL.Text (ReaderT LCIState IO) a


getProjects = fmap projects $ lift ask

freshId :: Action Int
freshId  = do tv <- fmap counter $ lift ask
              modifyTVar tv (+1)

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

blaze :: H.Html -> Action ()
blaze = html . renderHtml
