{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import
import Model.Users

import Yesod.Form.Bootstrap3
import Text.Blaze.Html5



getUsersR :: Handler Html
getUsersR = do
    us <- runDB $ do
        selectList [] [Asc UserFirstname]
    -- let mus = listToMaybe us
    (formWidget, formEnctype) <- generateFormPost newUserForm
    defaultLayout $ do
        setTitle "Users"
        $(widgetFile "users")


postUsersR :: Handler Html
postUsersR = do
    ((result, formWidget), formEnctype) <- runFormPost newUserForm
    
    us <- runDB $ do
        case result of
            FormFailure xs -> do
                setMessage (ul . toHtml $ Import.map (li.toHtml) xs)
            FormSuccess user -> do
                _ <- insert user
                return ()
            FormMissing ->
                setMessage "No form data"
        selectList [] [Asc UserFirstname]
            
    defaultLayout $ do
        setTitle "Users"
        $(widgetFile "users")


newUserForm' :: Form Text
newUserForm' = renderDivs $ areq textField "New user's name:" Nothing

newUserForm :: Form User
newUserForm = renderBootstrap3 formLayout
    $ User
    <$> areq emailField  (bfs' "Email:") Nothing
    <*> areq textField  (bfs' "First name:") Nothing
    <*> areq textField  (bfs' "Last name:") Nothing
    <*> aopt textField  (bfs' "Nickname:") Nothing
    <*> pure Nothing
    <*> pure Nothing
    <*> pure False
    <*  (bootstrapSubmit $ BootstrapSubmit ("Add User" :: Text) "" [])
    


getUserR :: UserId -> Handler Html
getUserR uid = do
    user <- runDB $ get404 uid
    defaultLayout [whamlet|
        <h1>Hello #{userShortName user}!
        Nice to see you!
        |]