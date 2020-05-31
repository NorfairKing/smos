module Main where

import Prelude
import Data.Foldable (traverse_)
import Control.Monad.State
import Data.Maybe (Maybe(..))
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Web.HTML.HTMLElement as WHHE
import Web.UIEvent.KeyboardEvent as WUEK
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
  = { contents :: String, selected :: Boolean }

data Action
  = ChangeString String
  | Select Boolean

textboxComponent :: forall q i o. H.Component HH.HTML q i o Aff
textboxComponent =
  H.mkComponent
    { initialState: \_ -> { contents: "start", selected: false }
    , render: renderTextboxComponent
    , eval: H.mkEval $ H.defaultEval { handleAction = handleTextboxComponentAction }
    }

renderTextboxComponent :: forall m. State -> H.ComponentHTML Action () m
renderTextboxComponent state =
  if state.selected then
    HH.input
      [ HE.onValueInput \t -> Just (ChangeString t)
      , HE.onKeyDown (\ke -> if WUEK.code ke == "Enter" then Just (Select false) else Nothing)
      , HP.placeholder state.contents
      , HP.ref (H.RefLabel "textbox")
      ]
  else
    HH.p
      [ HE.onClick $ \_ -> Just (Select true)
      ]
      [ HH.text state.contents
      ]

handleTextboxComponentAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleTextboxComponentAction a = case a of
  ChangeString t -> modify_ (\s -> s { contents = t })
  Select b -> do
    modify_ (\s -> s { selected = b })
    H.getHTMLElementRef (H.RefLabel "textbox")
      >>= traverse_ \element -> do
          H.liftEffect (WHHE.focus element)
