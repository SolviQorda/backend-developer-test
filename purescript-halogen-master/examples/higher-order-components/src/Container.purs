module Example.HOC.Container where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Exception as Exn
import Example.HOC.Button as Button
import Example.HOC.HOC as HOC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandleButton Button.Message a
  | CheckButtonState a

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

type ChildSlots =
  ( button :: H.Slot Button.Query Button.Message Unit
  )

_button = SProxy :: SProxy "button"

component :: forall m. MonadError Exn.Error m => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState =
    { toggleCount: 0
    , buttonState: Nothing
    }

  render :: State -> H.ComponentHTML Query ChildSlots m
  render state =
    HH.div_
      [ HH.slot _button unit (HOC.factory Button.myButton) true (HE.input HandleButton)
      , HH.p_
          [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
      , HH.p_
          [ HH.text
              $ "Last time I checked, the button was: "
              <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
              <> ". "
          , HH.button
              [ HE.onClick (HE.input_ CheckButtonState) ]
              [ HH.text "Check now" ]
          ]
      ]

  eval :: Query ~> H.HalogenM State Query ChildSlots Void m
  eval = case _ of
    HandleButton (Button.Toggled _) next -> do
      H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    CheckButtonState next -> do
      buttonState <- H.query _button unit $ H.request Button.IsOn
      H.modify_ (_ { buttonState = buttonState })
      pure next
