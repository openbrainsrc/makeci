module SpockWorker where

import Web.Spock
import Control.Concurrent.STM 
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Pool


type Queue a = TVar [a]

type WorkerM conn a = ReaderT conn IO a

spockWorker :: Pool conn -> (a -> WorkerM (Pool conn) ()) -> IO (Queue a)
spockWorker conn worker = do
  jobqueue <- atomically $ newTVar []
  let bw = do nextTask <- atomically $ do 
                  jobs <- readTVar jobqueue
                  case jobs of 
                     [] -> retry
                     job:more_jobs -> do
                          writeTVar jobqueue more_jobs
                          return job
              runReaderT (worker nextTask) conn
              bw
  forkIO bw
  return jobqueue

readQueue :: MonadIO m => Queue a -> m [a]
readQueue q = liftIO $ atomically $ readTVar q

addJob :: MonadIO m => Queue a -> a -> m ()
addJob jobqueue x = liftIO $ atomically $ modifyTVar jobqueue (++[x])

runQueryW :: (conn -> IO b) -> WorkerM (Pool conn) b
runQueryW query = do
  pool <- ask
  liftIO (withResource pool query)