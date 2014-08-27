{-# LANGUAGE TupleSections #-}
module Handler.Calendar where

import Data.List.Split
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Import
import System.Locale (defaultTimeLocale)
import Data.Aeson
import Data.Text (unpack)
--import           Yesod.Form.Jquery

getCalendarR :: Handler Html
getCalendarR = do
    --now <- liftIO $ getCurrentTime
    --let today = utctDay now
    --    (year, month, _day) = toGregorian today
    --    (yearDelta, nextMon) = (month + 1) `quotRem` 12
    --    nextMonth = fromGregorian (year + fromIntegral yearDelta) nextMon 1

    --    firstOfTheMonth = fromGregorian year month 1
    --    (yr, wk, _) = toWeekDate firstOfTheMonth
    --    prevSunday  = fromWeekDate yr wk 0
    --    (nyr, nwk, _) = toWeekDate nextMonth
    --    finalSaturday = fromWeekDate nyr nwk 6

    --    weeks = chunksOf 7 $ map (formatTime defaultTimeLocale "%a %b %e") 
    --                        [prevSunday .. finalSaturday]


    --es' <- runDB $ selectList [EventDate >=. prevSunday, EventDate <=. finalSaturday]
    --                          [Asc EventDate, Desc EventTime]
    --es <- mapM (\u -> (u,) <$> newIdent) es'
    -- let mus = listToMaybe us
    --(formWidget, formEnctype) <- generateFormPost eventForm
    

    defaultLayout $ do
        setTitle "Calendar"
        addScript     $ StaticR js_moment_with_locales_js
        addScript     $ StaticR js_fullcalendar_2_1_0_fullcalendar_js
        addStylesheet $ StaticR js_fullcalendar_2_1_0_fullcalendar_css
        $(widgetFile "calendar")

getCalendarJsonR :: Handler Value
getCalendarJsonR = do
    start <- lookupGetParam "start"
    end   <- lookupGetParam "end" 
    liftIO $ print (start, end)

    let readM :: Read a => String -> Maybe a
        readM str = case reads str of
                        [(x,_)] -> Just x
                        _       -> Nothing

    let mse :: Maybe (Day, Day)
        mse = do
            s <- start
            e <- end
            (,) <$> readM (unpack s) <*> readM (unpack e)

        eventToJson :: Entity Event -> Value
        eventToJson (Entity i ev) =
            object [
                "id"    .= show (unKey i),
                "title" .= eventTitle ev,
                "start" .= (show $ eventDate ev)

            ]
    liftIO $ print mse
    case mse of
        Nothing -> return $ object []
        Just (start, end) -> do 
            es <- runDB $ do
                selectList [EventDate >=. start, EventDate <=. end]
                           [Asc EventDate, Desc EventTime]
            --liftIO $ print es
            return $ toJSON $ map eventToJson $ es
            --return $ object ["test" .= ("hello" :: String)]
