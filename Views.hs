{-# LANGUAGE OverloadedStrings #-}
module Views where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, td, tr)
import Text.Blaze.Html5 ((!))
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5.Attributes as A
import Control.Concurrent.STM hiding (modifyTVar, atomically)
import Data.Monoid
import Utils
import qualified Data.Time.Format as DTF
import System.Locale (defaultTimeLocale)
import Data.Time

import Database
import           Database.Persist hiding (get)


jobRow (Project u r, Entity jid (Job _ hash commit start mdone status out)) = 
    tr ! A.class_ (statusToClass status) $ do 
                     td $ toHtml $ "#" ++ show (entityToIntId jid)
                     td $ toHtml u >> "/" >> toHtml r
                     td $ showDateAndTime start
                     td $ toHtml hash
                     td $  toHtml commit
                     td $ H.a ! A.href (H.toValue $ "/job/"++show (entityToIntId jid)) $ showStatus start status

jobQRow (Project u r) = 
    tr ! A.class_ "warning" $ do 
                     td $ ""
                     td $ toHtml u >> "/" >> toHtml r
                     td $ ""
                     td $ ""
                     td $ ""
                     td $ ""
                     
statusToClass "Success" = "success"
statusToClass "BuildFailure" = "error"
statusToClass "TestFailure" = "error"
statusToClass "Pending" = "warning"
statusToClass _ = "info"

jobDisp prj job = 
  let htbody = jobOutput job
      status = jobStatus job
--  let outT = TL.unlines $ reverse $ map TL.pack outSS
  in template (projectRepoName prj) (return ()) $ do
    
            H.h3 $ do H.toHtml $ projectUserName prj
                                 ++ "/" 
                                 ++ projectRepoName prj
                                 ++ ": "
                      showStatus (jobStart job) (jobStatus job)
            htbody

projRow (Entity pid proj@(Project u r)) = 
 {-active_jobs <- getJobQueue
  done_jobs <- getJobsDone
  tclass <- case [job | job <- active_jobs++done_jobs, jobProj job == proj] of
               job:_ -> fmap statusToClass $ atomically $ readTVar $ jobStatus job 
               [] -> return "" -}
  let fullNm = u ++ "/"++ r
      url = "https://github.com/"++fullNm
  in tr $ do 
    td $ H.a ! A.href (H.toValue url) $ toHtml fullNm
    td $ H.a ! A.class_ "btn btn-mini" ! A.href (H.toValue $ "/build-now/"++show (entityToIntId pid)) $ "Build now"

template :: String -> H.Html -> H.Html -> H.Html
template title extra_head body_html = H.docTypeHtml $ do
        H.head $ do
          H.title $ H.toHtml title
          H.link ! A.href "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" ! A.rel "stylesheet"
                
          H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                 $ ""
          H.script ! A.src "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
                 $ ""
          extra_head
        H.body $ do
           H.div ! A.class_ "container" $ 
             H.div ! A.class_ "row" $ 
               H.div ! A.class_ "span12" $  body_html

showStatus :: UTCTime -> Status -> H.Html
{-showStatus start (Success) 
   = let durSecs = round (realToFrac $ diffUTCTime tm start)
         durMins = durSecs `div` 60
     in toHtml $ if durMins == 0 
                    then "Success ("++show durSecs++"s)"
                    else "Success ("++show durMins++"m"++show (durSecs `rem` 60) ++"s)" -}

showStatus _ s = toHtml $ s

showDateAndTime :: UTCTime -> H.Html
showDateAndTime t = 
  let spec = "%e-%b-%y %H:%M"
  in toHtml $ DTF.formatTime defaultTimeLocale spec t
