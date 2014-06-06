module Handler.Event where

import Import
import qualified Data.Text as T
import Data.Map.Strict as M
import Data.Set as S
import Model.Availability
import Handler.Users (userPrettyName)

getEventR :: EventId -> Handler Html
getEventR eid = do
    (evt,avs,us) <- runDB $ do
        evt <- get404 eid
        avs <- selectList [AvailabilityEvent ==. eid] []
        us  <- selectList [] [Asc UserFirstname]
        return (evt,avs,us)
    let amap :: M.Map Available [Entity Availability]
        amap =
            M.fromListWith (++)
            . (Import.map (\s -> (s,[])) [Yes, No, Unsure, Unset] ++)
            . Import.map (\(Entity avid avail) -> (availabilityStatus avail, [Entity avid avail]))
            $ avs
        umap :: M.Map UserId User
        umap =
            M.fromList
            . Import.map (\(Entity uid user) -> (uid,user))
            $ us
        -- uset :: S.Set UserId
        -- uset =
        --     S.fromList
        --     . Import.map (\(Entity uid _) -> uid)
        --     $ us
        yes, no, unsure :: [User]
        yes    = Import.map ((umap M.!) . availabilityUser . entityVal) $ amap ! Yes
        no     = Import.map ((umap M.!) . availabilityUser . entityVal) $ amap ! No
        unsure = Import.map ((umap M.!) . availabilityUser . entityVal) $ amap ! Unsure
        -- unset' = amap ! Unset 
        -- unset :: [UserId]
        -- unset  = (Import.map ( availabilityUser . entityVal) unset' ++)
        --     . Import.map (umap M.!)
        --     $ S.toList (uset S.\\ S.fromList seen)
        -- --         ++ S.toList
        -- --         $ uset S.\\ S.fromList []
        -- seen   = do
        --     l <- [yes, no, unsure, unset']
        --     Entity aid av <- l
        --     return (availabilityUser av)
        
    defaultLayout $ do
        setTitle . toHtml . T.concat $ ["Event - ", eventTitle evt]
        $(widgetFile "event")
    

postEventR :: EventId -> Handler Html
postEventR = error "Not yet implemented: postEventR"
