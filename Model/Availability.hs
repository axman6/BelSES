{-# LANGUAGE TemplateHaskell #-}
module Model.Availability
    (
      Available (..)
    , Period (..)
    , toggleAvailability
    , availableToText
    , availableToClass
    , availableToIcon
    )
 where


import Database.Persist.TH
import Yesod.Core.Dispatch
import Prelude (Show, Read, Eq, Ord, Maybe(..), ($))
import Data.Text (Text)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html5 (Html)

data Available = Unset | Yes | No | Unsure 
    deriving (Show, Read, Eq, Ord)

data Period = Day | Night
    deriving (Show, Read, Eq, Ord)

instance PathPiece Period where
    fromPathPiece t = case t of
        "Day" -> Just Day
        "Night" -> Just Night
        _ -> Nothing
    toPathPiece Day = "Day"
    toPathPiece Night = "Night"


toggleAvailability :: Available -> Available
toggleAvailability a = case a of
    Unset   -> Yes
    Yes     -> No
    No      -> Unsure
    Unsure  -> Unset 

availableToText :: Available -> Html
availableToText a = preEscapedToMarkup $ case a of
    Yes     -> ("Yes" :: Text)
    No      -> "No"
    Unsure  -> "Unsure"
    Unset   -> "&nbsp;&nbsp;&nbsp;"

availableToClass :: Available -> Text
availableToClass a = case a of
    Yes     ->"btn-success"
    No      ->"btn-danger" 
    Unsure  ->"btn-warning"
    Unset   ->"btn-default"


availableToIcon :: Available -> Text
availableToIcon a = case a of
    Yes     ->"glyphicon-ok-sign"
    No      ->"glyphicon-remove-sign" 
    Unsure  ->"glyphicon-question-sign"
    Unset   ->"glyphicon-minus"

derivePersistField "Available"
derivePersistField "Period"