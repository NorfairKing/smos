{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.API.Password
  ( module Data.Password.Bcrypt,
  )
where

import Autodocodec
import Data.Password.Bcrypt
import Data.Password.Instances ()
import Data.Validity

instance Validity Password where
  validate = trivialValidation

instance HasCodec Password where
  codec = dimapCodec mkPassword unsafeShowPassword codec
