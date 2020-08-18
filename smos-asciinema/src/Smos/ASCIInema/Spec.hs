{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.ASCIInema.Spec where

import Data.List
import Data.Yaml
import YamlParse.Applicative

data ASCIInemaCommand
  = Wait Word -- Milliseconds
  | SendInput String
  | Type String Word -- Milliseconds
  deriving (Show, Eq)

instance FromJSON ASCIInemaCommand where
  parseJSON = viaYamlSchema

instance YamlSchema ASCIInemaCommand where
  yamlSchema =
    alternatives
      [ objectParser "Wait" $ Wait <$> requiredField "wait" "How long to wait (in milliseconds)",
        objectParser "SendInput" $ SendInput <$> requiredField "send" "The input to send",
        objectParser "Type" $
          Type
            <$> requiredField "type" "The input to send"
            <*> optionalFieldWithDefault "delay" 100 "How long to wait between keystrokes (in milliseconds)"
      ]

commandDelay :: ASCIInemaCommand -> Word
commandDelay = \case
  Wait w -> w
  SendInput _ -> 0
  Type s i -> genericLength s * i
