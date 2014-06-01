module Handler.Event where

import Import
import qualified Data.Text as T

getEventR :: EventId -> Handler Html
getEventR eid = do
    evt <- runDB $ get404 eid
    defaultLayout $ do
        setTitle . toHtml . T.concat $ ["Event - ", eventTitle evt]
        $(widgetFile "event")
    

postEventR :: EventId -> Handler Html
postEventR = error "Not yet implemented: postEventR"
