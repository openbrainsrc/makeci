{-# LANGUAGE OverloadedStrings #-}
module Views where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, td, tr)
import Text.Blaze.Html5 ((!))
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5.Attributes as A
import Control.Concurrent.STM hiding (modifyTVar, atomically)
import Data.Monoid
import Types
import Utils

jobRow (Job (Project u r) id statusTV outTV) = do
    status <- atomically $ readTVar statusTV
    return $ TL.concat ["<tr><td>#", tshow id, 
                       "</td><td>", TL.pack u, 
                       "/", TL.pack r,"</td><td><a href=\"/job/",
                       tshow id,"\">",tshow status,"</a></td></tr>"]
jobDoneRow (Job (Project u r) id statusTV outTV) = do
    status <- atomically $ readTVar statusTV
    return $ tr $ do td $ toHtml $ "#" <> tshow id
                     td $ toHtml u >> "/" >> toHtml r
                     case status of
                       Success hash commit time
                         -> do td $ H.a ! A.href (H.toValue $ "/job/"++show id) $ toHtml (show time)
                               td $ toHtml hash
                               td $ toHtml commit
                       _ -> do td $ H.a ! A.href (H.toValue $ "/job/"++show id) $ toHtml (show status)
                               td ""
                               td ""

jobDisp job = do
  htbody <- atomically $ readTVar $ jobOutput job
  status <- atomically $ readTVar $ jobStatus job
--  let outT = TL.unlines $ reverse $ map TL.pack outSS
  return $ template (repoName $ jobProj job) $ do
     H.div ! A.class_ "container" $ do
       H.div ! A.class_ "row" $ do
         H.div ! A.class_ "span12" $ do
            H.h3 $ H.toHtml $ userName (jobProj job) 
                              ++ "/" 
                              ++ repoName (jobProj job)
                              ++ ": "
                              ++ show  status
            htbody

projRow (Project u r) 
  = return $ TL.concat ["<tr><td>", TL.pack u, 
                        "/", TL.pack r,
                        "</td><td><a href=\"/build-now/", 
                        TL.pack r,
                        "\">Build now</a></td><tr>"]

template title body_html = H.docTypeHtml $ do
        H.head $ do
          H.title $ H.toHtml title
          H.link ! A.href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" ! A.rel "stylesheet"
                
          H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                 $ ""
          H.script ! A.src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
                 $ ""
        H.body body_html
