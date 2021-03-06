{-# LANGUAGE OverloadedStrings #-}
module Views where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, td, tr)
import Text.Blaze.Html5 ((!))
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid
import Utils
import qualified Data.Time.Format as DTF
import System.Locale (defaultTimeLocale)
import Data.Time

import Database
import Database.Persist hiding (get)
import Database.Persist.Sql (fromSqlKey)


jobRow (Project u r, Entity jid (Job _ hash commit start mdone status out)) =
    tr ! A.class_ (statusToClass status) $ do
                     td $ toHtml $ "#" ++ show (fromSqlKey jid)
                     td $ toHtml u >> "/" >> toHtml r
                     td $ showDateAndTime start
                     td $ toHtml hash
                     td $ toHtml commit
                     td $ H.a ! A.href (H.toValue $ "/job/"++show (fromSqlKey jid)) $ toHtml status

jobQRow (Job _ hash commit start mdone status out) =
    tr ! A.class_ "warning" $ do
                     td $ ""
                     td $ ""
                     td $ showDateAndTime start
                     td $ toHtml hash
                     td $ toHtml commit
                     td $ toHtml status

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

            H.h3 $ do toHtml $ projectUserName prj
                                 ++ "/"
                                 ++ projectRepoName prj
                                 ++ ": "
                      toHtml $ jobStatus job
            htbody

projRow (Entity pid proj@(Project u r)) =
  let fullNm = u ++ "/"++ r
      url = "https://github.com/"++fullNm
  in tr $ do
    td $ H.a ! A.href (H.toValue url) $ toHtml fullNm
    td $ do H.a ! A.class_ "btn btn-mini" ! A.href (H.toValue $ "/clean/"++show (fromSqlKey pid)) $ "Clean"
            H.a ! A.class_ "btn btn-mini" ! A.href (H.toValue $ "/build-now/"++show (fromSqlKey pid)) $ "Build now"


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

showDateAndTime :: UTCTime -> H.Html
showDateAndTime t =
  let spec = "%e-%b-%y %H:%M"
  in toHtml $ DTF.formatTime defaultTimeLocale spec t
