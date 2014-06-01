module Handler.Avail where

import Import
import              Data.Time.Clock
import              Data.Time.LocalTime

getAvailR :: Handler Html
getAvailR = do
    now <- liftIO $ getCurrentTime
    (es,us) <- runDB $ 
        (,) <$> selectList
                    [EventDate >=. utctDay now]
                    [Asc EventDate, Asc EventTime, LimitTo 10]
            <*> selectList [] [Asc UserFirstname]
    defaultLayout $ do
        setTitle "Availability"
        $(widgetFile "availability")