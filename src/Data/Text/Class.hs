{-# OPTIONS_GHC -Wall #-}
module Data.Text.Class where

import qualified Data.Text as T

class ToText a where
    toText :: a -> T.Text

instance ToText Double where
    toText = T.pack . show

instance ToText Int where
    toText = T.pack . show
