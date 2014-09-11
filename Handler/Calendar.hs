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
    rend   <- getUrlRender

    let mse :: Maybe (Day, Day)
        mse = do
            s <- mstart
            e <- mend
            (,) <$> readM (unpack s) <*> readM (unpack e)

        eventToJson :: Entity Event -> Value
        eventToJson (Entity i ev) =
            let datetime :: Day -> TimeOfDay -> UTCTime
                datetime date time = UTCTime date (timeOfDayToTime time)

                dt :: UTCTime
                dt = datetime (eventDate ev) (eventTime ev)
                
                mendTime :: Maybe UTCTime
                mendTime = do
                        edate <- eventEndDate ev
                        etime <- eventEndTime ev
                        return $ (datetime edate etime)
            in
            object $ [
                 "id"    .= toPathPiece (unKey i)
                ,"title" .= eventTitle ev
                ,"start" .= isoDateTime dt
                --,"url"   .= rend (EventR i)
                ,"content" .= eventNotes ev
                -- End time is either the recorded end time
                -- or the start time plus an hour (and a second because)
                -- FullCalendar defines it as the first time AFTER the event
                -- Defaults to 1h from start time
                , "end" .= maybe (isoDateTime (addUTCTime 3601 dt))
                                 (\et -> isoDateTime (addUTCTime 1 et))
                                 mendTime
                ]
    
    case mse of
        Nothing -> return $ object []
        Just (start, end) -> do
            es <- runDB $
                selectList [FilterOr 
                                [FilterAnd 
                                    [EventDate >=. start,
                                     EventDate <=. end]
                                ,FilterAnd 
                                    [EventEndDate >=. Just start, 
                                     EventEndDate <=. Just end]
                                ]
                            ]
                           []
            return $ toJSON $ map eventToJson $ es
