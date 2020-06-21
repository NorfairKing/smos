module Main where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut.Common as CA
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import Halogen (liftEffect)
import Halogen.Aff (awaitBody, runHalogenAff, selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Editor as Editor
import Encoding (smosFileCodec)

foreign import getStartingJson :: Effect Json

main :: Effect Unit
main = do
  runHalogenAff do
    body <- awaitBody
    editorElement <- do
      me <- selectElement (QuerySelector "#editor")
      pure
        $ case me of
            Nothing -> body
            Just e -> e
    startingJson <- liftEffect getStartingJson
    case CA.decode smosFileCodec startingJson of
      Left err -> liftEffect (Console.log (CA.printJsonDecodeError err))
      Right startingFile -> void $ runUI (Editor.component startingFile) unit editorElement
