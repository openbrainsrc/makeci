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
import Web.Spock
import Web.Scotty
import Text.Blaze.Html5
import qualified Data.Text.Lazy as T
import SpockWorker

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    userName String
    repoName String
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
        runResourceT $ runNoLoggingT $ runSqlConn action conn

runDBw action =
    runQueryW $ \conn ->
        runResourceT $ runNoLoggingT $ runSqlConn action conn

instance Parsable (KeyBackend SqlBackend e) where
  parseParam t = case readsPrec 5 $ T.unpack t of 
                   [] -> Left $ "Cannot parse id from " `T.append` t
                   (x,_):_ -> Right $ Key $ toPersistValue (x::Int)