module Header where

import Prelude
import Data.Foldable (traverse_)
import Control.MonadZero (guard)
import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Web.HTML.HTMLElement as WHHE
import Web.UIEvent.KeyboardEvent as WUEK
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

type State
  = { contents :: String }

type Input
  = String

type Output
  = String

data Action
  = Init
  | ChangeString String
  | Commit

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: \s -> { contents: s }
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handle, initialize = Just Init }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.input
    [ HE.onValueInput \t -> Just (ChangeString t)
    , HE.onKeyDown
        ( \ke ->
            guard
              ( let
                  k = WUEK.code ke
                in
                  k == "Enter" || k == "Escape"
              )
              $> Commit
        )
    , HP.ref (H.RefLabel "textbox")
    , HP.value state.contents
    ]

handle :: Action -> H.HalogenM State Action () Output Aff Unit
handle a = case a of
  Init ->
    H.getHTMLElementRef (H.RefLabel "textbox")
      >>= traverse_ \element -> do
          H.liftEffect (WHHE.focus element)
  ChangeString t -> modify_ (\s -> s { contents = t })
  Commit -> do
    s <- H.get
    H.raise s.contents
