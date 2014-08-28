{-# LANGUAGE TupleSections #-}
module Handler.Calendar where

import           Data.Aeson
import           Data.Text                   (unpack)
import           Data.Time
import           Import

getCalendarR :: Handler Html
getCalendarR = do
    defaultLayout $ do
        setTitle "Calendar"
        addScript     $ StaticR js_moment_with_locales_js
        addScript     $ StaticR js_fullcalendar_2_1_0_fullcalendar_js
        addStylesheet $ StaticR js_fullcalendar_2_1_0_fullcalendar_css
        $(widgetFile "calendar")

getCalendarJsonR :: Handler Value
getCalendarJsonR = do
    mstart <- lookupGetParam "start"
    mend   <- lookupGetParam "end"
    rend <- getUrlRender

    let mse :: Maybe (Day, Day)
        mse = do
            s <- mstart
            e <- mend
            (,) <$> readM (unpack s) <*> readM (unpack e)

        eventToJson :: Entity Event -> Value
        eventToJson (Entity i ev) =
            object [
                "id"    .= show (unKey i),
                "title" .= eventTitle ev,
                "start" .= (show $ eventDate ev),
                "url"   .= rend (EventR i)
            ]
    case mse of
        Nothing -> return $ object []
        Just (start, end) -> do
            es <- runDB $ do
                selectList [EventDate >=. start, EventDate <=. end]
                           [Asc EventDate, Desc EventTime]
            return $ toJSON $ map eventToJson $ es
