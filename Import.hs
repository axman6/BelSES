module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>), (<*))
import           Data.Text            as Import (Text)
import           Data.Text            as T (length, append, take)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

import           Data.Maybe           as Import (fromMaybe)

import           Yesod.Form.Bootstrap3

import           System.Locale                  (defaultTimeLocale)
import           Data.Time
import           Data.Time.Format


#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

bfs' :: (RenderMessage site Text) => Text -> FieldSettings site
bfs' m = bfs m

formLayout :: BootstrapFormLayout
formLayout = BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)

shorten :: Int -> Text -> Text
shorten len txt =
	if T.length txt > len
		then T.take (len - 3) txt `T.append` "..."
		else txt

readM :: Read a => String -> Maybe a
readM str = case reads str of
                ((x,_):_) -> Just x
                _       -> Nothing

isoDateTime :: FormatTime t => t -> String
isoDateTime = formatTime defaultTimeLocale "%Y-%m-%dT%T%z"
