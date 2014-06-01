{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Users where

import Import

import Yesod.Form.Bootstrap3

userPrettyName :: User -> Text
userPrettyName u = 
    let nick = userNickname u in
    userFirstname u
    <> case nick of
        Nothing -> " "
        Just n -> " \"" <> n <> "\" "
    <> userLastname u

userShortName :: User -> Text
userShortName u =
    let nick = userNickname u in
    case nick of
        Nothing -> userFirstname u
        Just n -> n

getUsersR :: Handler Html
getUsersR = do
    mus <- runDB $ do
        Just <$> selectList [] [Asc UserFirstname]
    -- let mus = listToMaybe us
    (formWidget, formEnctype) <- generateFormPost newUserForm
    defaultLayout $ do
        setTitle "Users"
        $(widgetFile "users")


postUsersR :: Handler Html
postUsersR = do
    ((result, formWidget), formEnctype) <- runFormPost newUserForm
    let submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    mus <- case submission of
        Nothing -> return Nothing
        Just user -> runDB $ do
            _ <- insert user
            fmap Just $ selectList [] [Asc UserFirstname]
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