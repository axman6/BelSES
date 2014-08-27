module Handler.Documents where

import Import
import Yesod.Form.Bootstrap3
import Data.Time.Clock


getDocumentsR :: Handler Html
getDocumentsR = do
    docs <- runDB $ selectList [] [Asc DocumentTitle]
    ((_result, formWidget), formEnctype) <- runFormPost newDocumentForm
    defaultLayout $ do
        setTitle "Documents"
        $(widgetFile "documents")

postDocumentsR :: Handler Html
postDocumentsR = do
    now <- liftIO $ getCurrentTime
    ((result, formWidget), formEnctype) <- runFormPost newDocumentForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    
    mfile <- runDB $ do 
        case submission of
            Nothing -> return Nothing
            Just file -> do
                did <- insert (Document (fileName file) (fileContentType file) now False)
                return $ Just (file, did)

    case mfile of
        Nothing -> return ()
        Just (file,did) -> do
            liftIO $ fileMove file ("uploads/" ++ (show $ unKey did))
            runDB $ update did [DocumentWritten =. True]
    
    docs <- runDB $ selectList [] [Asc DocumentTitle]
    defaultLayout $ do
        setTitle "Documents"
        $(widgetFile "documents")


newDocumentForm :: Form FileInfo
newDocumentForm = renderBootstrap3 formLayout $
    fileAFormReq  (bfs' "Upload:")
    <*  bootstrapSubmit (BootstrapSubmit ("Upload File" :: Text) "" [])

-- newDocumentForm :: Form (...)
-- newDocumentForm = renderBootstrap3 formLayout $
    
