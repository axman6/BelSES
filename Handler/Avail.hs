module Handler.Avail where

import Import
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Map as M
import Model.Availability
import Handler.Users
import Data.Text (pack)

numEvents :: Integer
numEvents = 7


getAvailR :: Handler Html
getAvailR = do
    now <- liftIO $ getCurrentTime
    let today = utctDay now
    (us, rs) <- runDB $ do
        us <- selectList [] [Asc UserFirstname]
        rs <- selectList [DailyAvailabilityDate >=. today, DailyAvailabilityDate <. addDays numEvents today] []
        return (us,rs)
    
    let mp = M.fromList $ map (\(Entity _ (DailyAvailability u d std stn note)) -> ((u,d),(std, stn, note))) rs
        
        avInfo :: (UserId,Day) -> (Available, Available, Maybe Text)
        avInfo (uid,day) = case M.lookup (uid,day) mp of
            Nothing -> (Unset, Unset,Nothing)
            Just a  -> a

        days = [today .. addDays numEvents today]

    defaultLayout $ do
        setTitle "Availability"
        $(widgetFile "availability")


updateAvail :: Period -> DailyAvailability -> (DailyAvailability,Available)
updateAvail Day a =
    let newSt = toggleAvailability (dailyAvailabilityStatusDay a)
    in (a {dailyAvailabilityStatusDay   = newSt}, newSt)
updateAvail Night a = 
    let newSt = toggleAvailability (dailyAvailabilityStatusNight a)
    in (a {dailyAvailabilityStatusNight = newSt}, newSt)


postToggleDailyAvailR :: UserId -> Day -> Period -> Handler Text
postToggleDailyAvailR uid day per = do
    res <- runDB $ do
        ma <- getBy $ UniqueDailyAvailability uid day
        case ma of
            Nothing -> do
                _ <- insert $ case per of
                    Day   -> DailyAvailability uid day Yes Unset Nothing
                    Night -> DailyAvailability uid day Unset Yes Nothing
                return Yes
            Just (Entity aid a) -> do
                let (_,newStatus) = updateAvail per a
                update aid $ case per of
                        Day   -> [DailyAvailabilityStatusDay   =. newStatus]
                        Night -> [DailyAvailabilityStatusNight =. newStatus]
                return newStatus
    return . pack . show $ res

