module Handler.Event where

import Import
import qualified Data.Text as T
import Data.Map.Strict as M
import Model.Availability
import Handler.Users (userPrettyName)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Calendar

getEventR :: EventId -> Handler Html
getEventR eid = do
    -- (avs,us) <- runDB $ do
    evt <- runDB $ do
        evt <- get404 eid
        -- avs <- selectList [DailyAvailabilityDate ==. day] []
        -- us  <- selectList [] [Asc UserFirstname]
        -- return (avs,us)
        return evt
    -- let amapDay :: M.Map Available [Entity DailyAvailability]
    --     amapDay =
    --         M.fromListWith (++)
    --         . (Import.map (\s -> (s,[])) [Yes, No, Unsure, Unset] ++)
    --         . Import.map (\(Entity avid avail) -> (dailyAvailabilityStatusDay avail, [Entity avid avail]))
    --         $ avs
    --     
    --     amapNight :: M.Map Available [Entity DailyAvailability]
    --     amapNight =
    --         M.fromListWith (++)
    --         . (Import.map (\s -> (s,[])) [Yes, No, Unsure, Unset] ++)
    --         . Import.map (\(Entity avid avail) -> (dailyAvailabilityStatusNight avail, [Entity avid avail]))
    --         $ avs
    --     
    --     umap :: M.Map UserId (Entity User)
    --     umap =
    --         M.fromList
    --         . Import.map (\ue@(Entity uid _) -> (uid,ue))
    --         $ us
    -- 
    --     usersWhoAre :: Available -> M.Map Available [Entity DailyAvailability] -> [Entity User]
    --     usersWhoAre av amap =
    --         sortBy (comparing (userFirstname . entityVal)) 
    --         . Import.map ((umap M.!) . dailyAvailabilityUser . entityVal)
    --         $ amap ! av
    -- 
    --     
    --     fs :: [(Available, Text, Text)]
    --     fs = [(Yes,"Available", "success"), (Unsure,"Unsure", "warning"), (No,"Unavailable", "danger")]

        
    defaultLayout $ do
        setTitle . toHtml . T.concat $ ["Event - ", eventTitle evt]
        
        -- setTitle . toHtml . T.concat $ ["Event - ", T.pack . show $ day]
        $(widgetFile "event")
    

postEventR :: EventId -> Handler Html
postEventR = error "Not yet implemented: postEventR"
