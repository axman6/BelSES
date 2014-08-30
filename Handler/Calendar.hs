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
            let datetime date time = UTCTime date (timeOfDayToTime time)

                dt = datetime (eventDate ev) (eventTime ev)
                dtStr = isoDateTime $ dt
                
                mendTime = do
                        edate <- eventEndDate ev
                        etime <- eventEndTime ev
                        return $ isoDateTime (datetime edate etime)
            in
            object $ [
                "id"    .= show (unKey i),
                "title" .= eventTitle ev,
                "start" .= dtStr,
                "url"   .= rend (EventR i),
                "content" .= eventNotes ev
                -- End time is either the recorded end time
                -- or the start time plus an hour (and a second because)
                -- FullCalendar defines it as the first time AFTER the event
            ] ++ maybe ["end" .= isoDateTime (addUTCTime 3601 dt)] 
                        (\endTime -> ["end" .= endTime])
                        mendTime
    
    case mse of
        Nothing -> return $ object []
        Just (start, end) -> do
            es <- runDB $
                selectList [EventDate >=. start, EventDate <=. end]
                           [Asc EventDate, Desc EventTime]
            return $ toJSON $ map eventToJson $ es
