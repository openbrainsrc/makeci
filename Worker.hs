{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}

module Worker where

import Utils

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
import Database.Persist.Sqlite hiding (get)

import Web.Spock.Worker

gitUrl (Project user repo) = "git@github.com:"++user++"/"++repo++".git"

ensure_exists_or_pull proj = do
    ex <- doesDirectoryExist $ "/tmp/" ++ projectRepoName proj
    if ex 
       then return () -- void $ pull proj
       else void $ system $ "git clone " ++ gitUrl proj ++ " /tmp/"++projectRepoName proj

pull proj = do
  let nm = projectRepoName proj
  
  liftIO $ system $ "cd /tmp/" ++ nm ++ " && git pull"  


runBuild :: WorkHandler Connection sess st JobId
runBuild jobId =  do
  -- TODO we really need to be in some error monad here...
  Just job <- runDB $ get jobId
  Just prj <- runDB $ get $ jobProject job
  continueBuild jobId job prj

continueBuild jobId job prj = do
  
    let updateJ = runDB . update jobId

    updateJ [JobStatus =. "Pulling"]
    
    pull prj

    gitres <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("git log --oneline -1")

    let (hash, commit) = case gitres of 
             Left err -> ("unknown", "unknown")
             Right s -> span (/=' ') s             

    
    updateJ [JobStatus =. "Building",
             JobGitHash =. hash,
             JobGitCommit =. commit]

    res <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("make cibuild")
    case res of
      Left errS -> do extraOutput <- getExtraOutput errS
                      updateJ [JobOutput =. pre errS >> extraOutput,
                               JobStatus =. "BuildFailure"]
                      return WorkComplete
      Right resS -> do updateJ [JobOutput =. pre resS,
                                JobStatus =. "Testing"]  
                       res <- liftIO $ psh ("/tmp/"++projectRepoName prj) ("make citest")
                       let tres = case res of
                                   Left terrS -> terrrS
                                   Right sresS -> sresS

                       extraOutput <- getExtraOutput sresS
                       now <- liftIO $ getCurrentTime
                       updateJ [JobOutput =. (pre resS >> pre tres >> extraOutput),
                                JobStatus =. "Success",
                                JobFinished =. Just now]
                       return WorkComplete


getExtraOutput sresS = liftIO $ do
  let files = catMaybes $ map (stripPrefix "file://") $ lines sresS
  fmap (preEscapedText . mconcat) $ mapM T.readFile files

pre = H.pre . preEscapedString

done "Success" = True
done "TestFailure" = True
done "BuildFailure" = True
done _ = False

{-myCatch :: (MonadBase IO m, Exception e) => m a       -- ^ The computation to run
        -> (e -> m a) -- ^ Handler to invoke if an exception is raised
        -> m a
myCatch a handler = liftBase $ \runInIO ->
                    catch (runInIO a)
                            (\e -> runInIO $ handler e) -}