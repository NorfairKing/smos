{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Directory.Archive
  ( HideArchive (..),
  )
where

import Autodocodec
import Data.Validity
import GHC.Generics (Generic)
import OptEnvConf

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)

instance Validity HideArchive

instance HasCodec HideArchive where
  codec =
    dimapCodec
      ( \case
          True -> HideArchive
          False -> Don'tHideArchive
      )
      ( \case
          HideArchive -> True
          Don'tHideArchive -> False
      )
      codec

instance HasParser HideArchive where
  settingsParser =
    choice
      [ setting
          [ help "Hide archived files",
            switch HideArchive,
            long "hide-archive"
          ],
        setting
          [ help "Do not hide archived files",
            switch Don'tHideArchive,
            long "show-archive",
            short 'a'
          ],
        setting
          [ help "Whether to consider archived entries",
            reader $
              eitherReader $
                \case
                  "True" -> Right HideArchive
                  "False" -> Right Don'tHideArchive
                  _ -> Left "Must be 'True' or 'False' if set",
            env "IGNORE_ARCHIVE",
            conf "hide-archive",
            metavar "BOOL"
          ]
      ]
