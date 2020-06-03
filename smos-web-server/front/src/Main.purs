module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import TreeCursor as TreeCursor

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI TreeCursor.component unit body
