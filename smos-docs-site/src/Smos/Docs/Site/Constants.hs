{-# LANGUAGE CPP #-}

module Smos.Docs.Site.Constants where

development :: Bool
#ifdef DEVELOPMENT
development = True
#else
development = False
#endif
