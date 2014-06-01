module Handler.Document where

import Import

getDocumentR :: DocumentId -> Handler Html
getDocumentR = error "Not implemented yet: getDocumentR"
    

postDocumentR :: DocumentId -> Handler Html
postDocumentR = error "Not yet implemented: postDocumentR"

getDocumentDownloadR :: DocumentId -> Handler TypedContent
getDocumentDownloadR = error "Not implemented yet: getDocumentDownloadR"
