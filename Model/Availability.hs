{-# LANGUAGE TemplateHaskell #-}
module Model.Availability
    (
    Available (..)
    )
 where

import Database.Persist.TH
import Prelude (Show, Read, Eq)

data Available = Yes | No | Unsure deriving (Show, Read, Eq)

derivePersistField "Available"