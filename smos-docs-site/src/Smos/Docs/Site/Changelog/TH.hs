{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Language.Haskell.TH.Syntax
import Path
import qualified System.FilePath as FP

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

deriving instance Lift Day

data Changelog = Changelog
  { changelogContents :: !Text,
    changelogRendered :: !Text
  }
  deriving (Typeable, Lift)

unreleasedFunc :: Text -> Maybe Changelog
unreleasedFunc contents =
  if T.null (T.strip contents)
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
