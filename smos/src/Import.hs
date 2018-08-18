{-# LANGUAGE PatternSynonyms #-}

module Import
    ( module X
    ) where

import Prelude as X hiding (head, init, last, tail)

import Debug.Trace as X

import GHC.Generics as X (Generic)

import Data.ByteString as X (ByteString)
import Data.Foldable as X
import Data.Function as X
import Data.Functor.Contravariant as X
import Data.List as X
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Map as X (Map)
import Data.Maybe as X
import Data.Monoid as X
import Data.Sequence as X
    ( pattern (:<|)
    , pattern (:|>)
    , pattern Empty
    , Seq(..)
    , (<|)
    , (|>)
    )
import Data.String as X
import Data.Text as X (Text)
import Data.Validity as X
import Data.Validity.Containers as X ()
import Data.Validity.HashMap as X ()
import Data.Validity.Text as X ()
import Data.Validity.Time as X ()

import Control.Arrow as X
import Control.Monad as X
import Control.Monad.IO.Class as X

import Text.Show.Pretty as X (ppShow)

import Path as X
import Path.IO as X

import Safe as X
