module Handler.Avail where

import Import
import Data.Time.Clock
import qualified Data.Map as M
import Model.Availability
import Handler.Users
import Data.Text (pack)

getAvailR :: Handler Html
getAvailR = do
    now <- liftIO $ getCurrentTime
    (es,us, rs) <- runDB $ do
        es <- selectList
                    [EventDate >=. utctDay now]
                    [Asc EventDate, Asc EventTime, LimitTo 10]
        us <- selectList [] [Asc UserFirstname]
        rs <- selectList [AvailabilityEvent <-. map entityKey es] []
        return (es,us,rs)
    
    let mp = M.fromList $ map (\(Entity _ (Availability u e st note)) -> ((u,e),(st,note))) rs
        
        avInfo :: (UserId,EventId) -> (Text, Text, Maybe Text)
        avInfo (uid,eid) = case M.lookup (uid,eid) mp of
            Nothing ->             ("btn-default"   ,"&nbsp;&nbsp;&nbsp;"   ,Nothing)
            Just (Yes,    note) -> ("btn-success"   ,"Yes"                  ,note)
            Just (No,     note) -> ("btn-danger"    ,"No"                   ,note)
            Just (Unsure, note) -> ("btn-warning"   ,"Unsure"               ,note)
            Just (Unset,  note) -> ("btn-default"   ,"&nbsp;&nbsp;&nbsp;"   ,note)

    defaultLayout $ do
        setTitle "Availability"
        $(widgetFile "availability")


postToggleAvailR :: UserId -> EventId -> Handler Text
postToggleAvailR uid eid = do
    res <- runDB $ do
        ma <- getBy $ UniqueAvailability uid eid
        case ma of
            Nothing -> do
                _ <- insert $ Availability uid eid Yes Nothing
                return Yes
            Just (Entity aid a) -> do
                let newStatus = toggleAvailability (availabilityStatus a)
                update aid
                    [AvailabilityStatus =. newStatus]
                return newStatus
    return . pack . show $ res

