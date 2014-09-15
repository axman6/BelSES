module Model.Users where

import Model
import Data.Text
import Prelude (Maybe(..))


#if __GLASGOW_HASKELL__ >= 704
import Data.Monoid (Monoid, (<>))
#else
import Data.Monoid (Monoid)
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif


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
        Nothing -> userFirstname u <> " " <> take 1 (userLastname u) <> "."
        Just n -> "\"" <> n <> "\""
