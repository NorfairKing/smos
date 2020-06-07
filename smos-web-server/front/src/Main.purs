module Main where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut.Common as CA
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Halogen (liftEffect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import TreeCursor as TreeCursor
import TreeEncoding (smosFileCodec)

foreign import getStartingJson :: Effect Json

main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    startingJson <- liftEffect getStartingJson
    case CA.decode smosFileCodec startingJson of
      Left err -> liftEffect (Console.log (CA.printJsonDecodeError err))
      Right startingFile -> void $ runUI (TreeCursor.component startingFile) unit body
