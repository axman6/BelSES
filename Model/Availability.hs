{-# LANGUAGE TemplateHaskell #-}
module Model.Availability
    (
      Available (..)
    , toggleAvailability
    )
 where

import Database.Persist.TH
import Prelude (Show, Read, Eq)

data Available = Unset | Yes | No | Unsure deriving (Show, Read, Eq)

toggleAvailability :: Available -> Available
toggleAvailability a = case a of
    Unset -> Yes
    Yes -> No
    No -> Unsure
    Unsure -> Unset 

derivePersistField "Available"