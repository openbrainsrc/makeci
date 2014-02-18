module Worker where

import Utils
import Types

import qualified Control.Concurrent.STM as STM
import Data.Maybe
import Data.List
import Text.Blaze.Internal (preEscapedText, preEscapedString)
import Control.Monad
import Control.Concurrent.STM hiding (modifyTVar, atomically)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.Cmd
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Trans
import System.Directory
import Data.Time

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
  out <- atomically $ newTVar (mempty)
  pull proj

  now <- liftIO $ getCurrentTime
  gitres <- liftIO $ psh ("/tmp/"++repoName proj) ("git log --oneline -1")
  let (hash, commit) = case gitres of 
             Left err -> ("unknown", "unknown")
             Right s -> span (/=' ') s
                        
  let job = Job proj jid hash  commit now status out
  withState jobQueue $ \jqtv -> do
       void $ modifyTVar jqtv (job:)  


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
                                
runBuild job@(Job prj jid _ _ _ statusTV outTV) =  do 
    STM.atomically $ writeTVar statusTV Building
    res <- psh ("/tmp/"++repoName prj) ("make cibuild")
    case res of
      Left errS -> STM.atomically $ do
                      writeTVar outTV $ H.pre $ preEscapedString errS
                      writeTVar statusTV BuildFailure
      Right resS -> do STM.atomically $ do 
                          writeTVar statusTV Testing
                          writeTVar outTV $ H.pre $ preEscapedString resS
                       res <- psh ("/tmp/"++repoName prj) ("make citest")
                       case res of
                         Left terrS -> STM.atomically $ do
                            writeTVar outTV $ do H.pre $ preEscapedString resS
                                                 H.pre $ preEscapedString terrS
                            writeTVar statusTV TestFailure
                         Right sresS -> do
                            extraOutput <- getExtraOutput sresS
                            now <- liftIO $ getCurrentTime
                            STM.atomically $ do
                              writeTVar outTV $ do H.pre $ preEscapedString resS
                                                   H.pre $ preEscapedString sresS
                                                   extraOutput
                              writeTVar statusTV $ Success now

getExtraOutput sresS = do
  let files = catMaybes $ map (stripPrefix "file://") $ lines sresS
  fmap (preEscapedText . mconcat) $ mapM T.readFile files
