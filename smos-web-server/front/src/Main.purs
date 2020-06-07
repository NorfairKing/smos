module Main where

import Data.Argonaut.Core
import Data.Codec.Argonaut.Common as CA
import Data.Either
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Console as Console
import Halogen (liftEffect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Prelude
import TreeCursor as TreeCursor
import TreeEncoding

foreign import getStartingJson :: Effect Json

main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    startingJson <- liftEffect getStartingJson
    case CA.decode smosFileCodec startingJson of
      Left err -> liftEffect (Console.log (CA.printJsonDecodeError err))
      Right startingFile -> do
        traceM startingJson
        void $ runUI (TreeCursor.component startingFile) unit body
