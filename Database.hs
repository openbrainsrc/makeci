{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import           Database.Persist hiding (get)
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

import Data.Time

import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Conduit.Pool
import Web.Spock
import Web.Scotty
import Text.Blaze.Html5
import qualified Data.Text.Lazy as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    userName String
    repoName String
    deriving Eq
Job 
    project ProjectId
    gitHash String
    gitCommit String
    start UTCTime
    finished UTCTime Maybe
    status String
    output Html
Session
    validUntil UTCTime
    userId Int
    deriving Show

|]

type Status = String

runDB action =
    runQuery $ \conn ->
        runResourceT $ runStderrLoggingT $ runSqlConn action conn 

runDB_io pool action = 
      withResource pool $ \conn -> 
         runResourceT $ runStderrLoggingT $ runSqlConn action conn 

instance Parsable (KeyBackend SqlBackend e) where
  parseParam t = case readsPrec 5 $ T.unpack t of 
                   [] -> Left $ "Cannot parse id from " `T.append` t
                   (x,_):_ -> Right $ Key $ toPersistValue (x::Int)

