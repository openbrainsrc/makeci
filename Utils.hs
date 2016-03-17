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
import qualified Data.Text as T
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist hiding (get)
import Shelly
import Data.String (fromString)


tshow :: Show a => a -> T.Text
tshow = T.pack . show

{-entityToIntId :: KeyBackend b e -> Int
entityToIntId ent = do
  case fromPersistValue . unKey $ ent of
    Right (uid::Int) ->  uid -}


psh :: String -> String -> IO (Either String String)
psh pwd cmds = shelly $ do
  cd $ fromString pwd
  let (cmd:args) = words cmds

  sout <- errExit False $ escaping False $ silently $ run (fromString cmd) (map T.pack args)
  serr <- lastStderr
  exCode <- lastExitCode
  let retS = (T.unpack $ T.append sout serr)
  if exCode == 0
    then return $ Right retS
    else return $ Left retS


-- from Baysig.Utils, by Ian Ross
pshIR :: String -> String -> IO (Either String String)
pshIR pwd cmd =
  myCatch $
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

myCatch :: IO (Either String a) -> IO (Either String a)
myCatch action = catch action $ \e -> return $ Left $ show (e :: SomeException)
