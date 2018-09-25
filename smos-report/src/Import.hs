{-# LANGUAGE PatternSynonyms #-}

module Import
    ( module X
    ) where

import Prelude as X hiding (head, init, last, tail)

import Data.Maybe as X

import Path as X
import Path.IO as X
