module Header where

import Control.Monad.State (modify_)
import Control.MonadZero (guard)
import Data.Foldable (traverse_, for_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude
import Web.HTML.HTMLElement as WHHE
import Web.HTML.HTMLInputElement as WHHIE
import Web.UIEvent.KeyboardEvent as WUEK
import Web.Event.Event as WEE
import Web.UIEvent.MouseEvent as WUEM

type State
  = { contents :: String, startingPosition :: StartingPosition }

type Input
  = { contents :: String, startingPosition :: StartingPosition }

data StartingPosition
  = Beginning
  | End

type Output
  = String

data Action
  = Init
  | Clicked WUEM.MouseEvent
  | ChangeString String
  | Commit

component :: forall q. H.Component HH.HTML q Input Output Aff
component =
  H.mkComponent
    { initialState: identity
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
                  k = WUEK.key ke
                in
                  k == "Enter" || k == "Escape"
              )
              $> Commit
        )
    , HP.ref (H.RefLabel "textbox")
    , HE.onClick \me -> Just (Clicked me)
    , HP.value state.contents
    ]

handle :: Action -> H.HalogenM State Action () Output Aff Unit
handle a = case a of
  Init ->
    H.getHTMLElementRef (H.RefLabel "textbox")
      >>= traverse_ \element ->
          for_ (WHHIE.fromHTMLElement element) \inputElement -> do
            s <- H.get
            let
              index = case s.startingPosition of
                Beginning -> 0
                End -> String.length s.contents
            H.liftEffect do
              WHHE.focus element
              WHHIE.setSelectionEnd index inputElement
  Clicked me -> H.liftEffect $ WEE.stopPropagation (WUEM.toEvent me)
  ChangeString t -> modify_ (\s -> s { contents = t })
  Commit -> do
    s <- H.get
    H.raise s.contents
