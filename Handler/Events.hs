{-# LANGUAGE TupleSections #-}
module Handler.Events where

import Import
import Data.Time.Clock
import Yesod.Form.Jquery
import Yesod.Form.Bootstrap3


getEventsR :: Handler Html
getEventsR = do
    now <- liftIO $ getCurrentTime
    es' <- runDB $ selectList [EventDate >=. utctDay now] [Asc EventDate, Asc EventTime, LimitTo 20]
    es <- mapM (\u -> (u,) <$> newIdent) es'
    -- let mus = listToMaybe us
    (formWidget, formEnctype) <- generateFormPost eventForm
    defaultLayout $ do
        setTitle "Events"
        $(widgetFile "events")

    

postEventsR :: Handler Html
postEventsR = do
    now <- liftIO $ getCurrentTime
    ((result, formWidget), formEnctype) <- runFormPost eventForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    es' <- runDB $ do 
        case submission of
            Nothing -> return ()
            Just event -> insert event >> return ()
        selectList [EventDate >=. utctDay now] [Asc EventDate, Asc EventTime, LimitTo 10]
    es <- mapM (\u -> (u,) <$> newIdent) es'
    defaultLayout $ do
        setTitle "Events"
        $(widgetFile "events")




eventForm :: Html -> MForm Handler (FormResult Event, Widget)
eventForm = renderBootstrap3 formLayout
    $ Event
     <$> areq textField  (bfs' "Title") (Just "Training")
     <*> areq (jqueryDayField def
         { jdsChangeYear = True -- give a year dropdown
         , jdsYearRange = "2010:+30" -- 1900 till five years ago
         })  (bfs' "Date") Nothing
     <*> areq timeField (bfs' "Time") Nothing
     <*> aopt textField  (bfs' "Location") Nothing
     <*> aopt urlField  (bfs' "Link") Nothing
     <*> (fmap unTextarea <$> aopt textareaField  (bfs' "Notes") (Just Nothing))
      <*  (bootstrapSubmit $ BootstrapSubmit ("Add Event" :: Text) "" [])
