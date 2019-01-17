module UiTest where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse

type State =
  { loading :: Boolean
  , profile ::
      { name :: String
      , longitude :: Number
      , latitude :: Number
      , games :: Array String
      , host :: Boolean
      }
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
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
    { loading: false
    , profile:
      { name: ""
      , longitude: 0.0
      , latitude: 0.0
      , games: []
      , host: false
      }
    , result: Nothing
    }

  render :: forall m. State -> H.ComponentHTML Query () m
  render st =
    HH.div_ $
      [ HH.form_  $
          [ HH.h1_ [ HH.text "Profile" ]
          , HH.label_
              [ HH.div_ [ HH.text "Name:" ]
              , HH.input
                  [ HP.value st.name
                  , HE.onValueInput (HE.input SetProfileName)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Longitude:" ]
              , HH.input
                  [ HP.value st.longitude
                  , HE.onValueInput (HE.input SetProfileLongitude)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Latitude:" ]
              , HH.input
                  [ HP.value st.latitude
                  , HE.onValueInput (HE.input SetProfileLatitude)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Games:" ]
              , HH.input
                  [ HP.value st.games
                  , HE.onValueInput (HE.input SetProfileGames)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Age:" ]
              , HH.input
                  [ HP.value st.age
                  , HE.onValueInput (HE.input SetProfileAge)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Host:" ]
              , HH.input
                  [ HP.type_ InputCheckbox
                  , HP.value st.host
                  , HE.onValueInput (HE.input SetProfileHost)
                  ]
              ]
          , HH.button
              [ HP.disabled st.loading
              , HP.type_ HP.ButtonButton
              , HE.onClick (HE.input_ MakeRequest)
              ]
              [ HH.text "Fetch info" ]
          , HH.p_
              [ HH.text (if st.loading then "Working..." else "") ]
          , HH.div_
              case st.result of
                Nothing -> []
                Just res ->
                  [ HH.h2_
                      [ HH.text "Response:" ]
                  , HH.pre_
                      [ HH.code_ [ HH.text res ] ]
                  ]
          ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void Aff
  eval = case _ of
    SetUsername username next -> do
      H.modify_ (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXResponse.string ("https://api.github.com/users/" <> username)
      H.modify_ (_ { loading = false, result = Just response.response })
      pure next
