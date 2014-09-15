module Handler.Event where

import Import
import qualified Data.Text as T
import Data.Map.Strict as M
import Model.Availability
import Model.Users (userPrettyName)
import Data.List (sortBy)
import Data.Ord (comparing)

getEventR :: EventId -> Handler Html
getEventR eid = do
    -- (avs,us) <- runDB $ do
    (evt,avs,us) <- runDB $ do
        evt <- get404 eid
        avs <- selectList [EventAvailabilityEvent ==. eid] []
        us  <- selectList [] [Asc UserFirstname]
        return (evt,avs,us)
        -- return evt
    let evmap :: M.Map Available [Entity EventAvailability]
        evmap =
            M.fromListWith (++)
            . (Import.map (\s -> (s,[])) [Yes, No, Unsure, Unset] ++)
            . Import.map (\(Entity eavid eavail) -> (eventAvailabilityStatus eavail, [Entity eavid eavail]))
            $ avs

        umap :: M.Map UserId (Entity User)
        umap =
            M.fromList
            . Import.map (\ue@(Entity uid _) -> (uid,ue))
            $ us
    
        usersWhoAre :: Available -> M.Map Available [Entity EventAvailability] -> [Entity User]
        usersWhoAre av amap =
            sortBy (comparing (userFirstname . entityVal)) 
            . Import.map ((umap M.!) . eventAvailabilityUser . entityVal)
            $ amap ! av
    
        
        fs :: [(Available, Text, Text)]
        fs = [(Yes,"Available", "success"), (Unsure,"Unsure", "warning"), (No,"Unavailable", "danger")]

        
    defaultLayout $ do
        setTitle . toHtml . T.concat $ ["Event - ", eventTitle evt]
        
        -- setTitle . toHtml . T.concat $ ["Event - ", T.pack . show $ day]
        $(widgetFile "event")
    

postEventR :: EventId -> Handler Html
postEventR = error "Not yet implemented: postEventR"
