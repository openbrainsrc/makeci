{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Utils where

import System.Process
import System.IO
import System.Exit
import qualified Control.Exception as C
import Control.Exception
import Control.DeepSeq (rnf)
import Control.Concurrent
import qualified Data.Text.Lazy as TL
import           Database.Persist.Sqlite hiding (get)


system' cmd = do
  putStrLn cmd
  res <- system cmd
  case res of
    ExitSuccess -> return ()
    ExitFailure f -> fail $ "error "++show f++": "++cmd


tshow :: Show a => a -> TL.Text
tshow = TL.pack . show

mostlyEmpty :: String -> Bool
mostlyEmpty s = all (`elem` " \n\r\t") s


entityToIntId :: PersistEntity e => Key e -> Int
entityToIntId ent = do
  case fromPersistValue . head . keyToValues $ ent of
    Right (uid::Int) ->  uid

force_psh :: String -> IO String
force_psh cmd = do
  res <- psh "." cmd
  case res of
   Left e -> fail e
   Right s -> return s



-- from Baysig.Utils, by Ian Ross


psh :: String -> String -> IO (Either String String)
psh pwd cmd =
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
