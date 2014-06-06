{-# LANGUAGE TemplateHaskell #-}
module Model.Availability
    (
      Available (..)
    , toggleAvailability
    )
 where

import Database.Persist.TH
import Prelude (Show, Read, Eq, Ord)

data Available = Unset | Yes | No | Unsure 
    deriving (Show, Read, Eq, Ord)

toggleAvailability :: Available -> Available
toggleAvailability a = case a of
    Unset -> Yes
    Yes -> No
    No -> Unsure
    Unsure -> Unset 

derivePersistField "Available"