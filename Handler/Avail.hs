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
    
    let mp = M.fromList $ map (\(Entity _ (Availability u e std stn note)) -> ((u,e),(std, stn, note))) rs
        
        avInfo :: (UserId,EventId) -> (Available, Available, Maybe Text)
        avInfo (uid,eid) = case M.lookup (uid,eid) mp of
            Nothing -> (Unset, Unset,Nothing)
            Just a  -> a


    defaultLayout $ do
        setTitle "Availability"
        $(widgetFile "availability")


updateAvail :: Period -> Availability -> (Availability,Available)
updateAvail Day a =
    let newSt = toggleAvailability (availabilityStatusDay a)
    in (a {availabilityStatusDay   = newSt}, newSt)
updateAvail Night a = 
    let newSt = toggleAvailability (availabilityStatusNight a)
    in (a {availabilityStatusNight = newSt}, newSt)


postToggleAvailR :: UserId -> EventId -> Period -> Handler Text
postToggleAvailR uid eid per = do
    res <- runDB $ do
        ma <- getBy $ UniqueAvailability uid eid
        case ma of
            Nothing -> do
                _ <- case per of
                    Day   -> insert $ Availability uid eid Yes Unset Nothing
                    Night -> insert $ Availability uid eid Unset Yes Nothing
                return Yes
            Just (Entity aid a) -> do
                let (_,newStatus) = updateAvail per a
                update aid $ case per of
                        Day   -> [AvailabilityStatusDay   =. newStatus]
                        Night -> [AvailabilityStatusNight =. newStatus]
                return newStatus
    return . pack . show $ res

