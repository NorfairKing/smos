module Main where

import Prelude
import Control.Monad.State (put)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI textboxComponent unit body

type State
  = String

data Action
  = ChangeString String

textboxComponent :: forall q i o m. H.Component HH.HTML q i o m
textboxComponent =
  H.mkComponent
    { initialState: \_ -> "start"
    , render: renderTextboxComponent
    , eval: H.mkEval $ H.defaultEval { handleAction = handleTextboxComponentAction }
    }

renderTextboxComponent :: forall m. String -> H.ComponentHTML Action () m
renderTextboxComponent state =
  HH.div
    []
    [ HH.input
        [ HE.onValueInput \t -> Just (ChangeString t), HP.placeholder state ]
    , HH.p
        []
        [ HH.text state ]
    ]

handleTextboxComponentAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleTextboxComponentAction = case _ of
  ChangeString t -> put t
