{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Docs.Site.Changelog.TH where

import CMarkGFM as MD
import Data.Data
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Path
import qualified System.FilePath as FP

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

deriving instance Data Rel

deriving instance Data File

deriving instance Lift Day

data Changelog = Changelog
  { changelogContents :: !Text,
    changelogRendered :: !Text
  }
  deriving (Show, Eq, Generic, Data, Typeable, Lift)

unreleasedFunc :: Text -> Maybe Changelog
unreleasedFunc contents =
  if T.strip contents == ""
    then Nothing
    else Just $ rawChangelogFunc contents

changelogKeyFunc :: Path Rel File -> Day
changelogKeyFunc rf = fromMaybe (error $ "not a valid day: " <> fromRelFile rf) $ parseTimeM True defaultTimeLocale "%F" $ FP.dropExtension $ fromRelFile rf

changelogValFunc :: Day -> Text -> Changelog
changelogValFunc _ = rawChangelogFunc

rawChangelogFunc :: Text -> Changelog
rawChangelogFunc contents =
  let rendered = MD.commonmarkToHtml [optUnsafe] [] contents
   in Changelog
        { changelogContents = contents,
          changelogRendered = rendered
        }
