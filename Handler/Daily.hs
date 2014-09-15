module Handler.Daily where

import Import
import Data.Map.Strict as M
import Model.Availability
import Model.Users (userPrettyName)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Calendar

getDailyR :: Day -> Handler Html
getDailyR day = do
    -- (avs,us) <- runDB $ do
    (mevt,avs,us) <- runDB $ do
        mevt <- getBy $ UniqueDailyInfo day
        avs  <- selectList [DailyAvailabilityDate ==. day] []
        us   <- selectList [] [Asc UserFirstname]
        return (mevt,avs,us)
    let amapDay :: M.Map Available [Entity DailyAvailability]
        amapDay =
            M.fromListWith (++)
            . (Import.map (\s -> (s,[])) [Yes, No, Unsure, Unset] ++)
            . Import.map (\(Entity avid avail) -> (dailyAvailabilityStatusDay avail, [Entity avid avail]))
            $ avs
        
        amapNight :: M.Map Available [Entity DailyAvailability]
        amapNight =
            M.fromListWith (++)
            . (Import.map (\s -> (s,[])) [Yes, No, Unsure, Unset] ++)
            . Import.map (\(Entity avid avail) -> (dailyAvailabilityStatusNight avail, [Entity avid avail]))
            $ avs
        
        umap :: M.Map UserId (Entity User)
        umap =
            M.fromList
            . Import.map (\ue@(Entity uid _) -> (uid,ue))
            $ us
    
        usersWhoAre :: Available -> M.Map Available [Entity DailyAvailability] -> [Entity User]
        usersWhoAre av amap =
            sortBy (comparing (userFirstname . entityVal)) 
            . Import.map ((umap M.!) . dailyAvailabilityUser . entityVal)
            $ amap ! av
    
        
        fs :: [(Available, Text, Text)]
        fs = [(Yes,"Available", "success"), (Unsure,"Unsure", "warning"), (No,"Unavailable", "danger")]

        
    defaultLayout $ do
        setTitle . toHtml . concat $ ["Availability - ", show $ day]
        $(widgetFile "daily")



