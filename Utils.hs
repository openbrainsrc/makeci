{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Utils where

import Control.Monad.Trans
import Control.Monad
import System.Process
import System.Cmd
import System.IO
import System.Exit
import qualified Control.Exception as C
import Control.Exception
import Control.DeepSeq (rnf)
import Control.Concurrent
import qualified Data.Text.Lazy as TL
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist hiding (get)



tshow :: Show a => a -> TL.Text
tshow = TL.pack . show

entityToIntId :: KeyBackend b e -> Int
entityToIntId ent = do
  case fromPersistValue . unKey $ ent of
    Right (uid::Int) ->  uid

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
        ExitFailure _ -> return $ Left (sout++serr))
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
