module Example.Components.Multitype.ComponentC where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = String

data Query a
  = HandleInput String a
  | GetValue (String -> a)

type Slot = H.Slot Query Void

component :: forall m. H.Component HH.HTML Query Unit Void m
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
  initialState = "Hello"

  render :: State -> H.ComponentHTML Query () m
  render state =
    HH.label_
      [ HH.p_ [ HH.text "What do you have to say?" ]
      , HH.input
          [ HP.value state
          , HE.onValueInput (HE.input HandleInput)
          ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void m
  eval (HandleInput value next) = do
    H.put value
    pure next
  eval (GetValue reply) = do
    reply <$> H.get
