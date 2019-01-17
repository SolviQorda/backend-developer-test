module Example.Effects.Aff.Ajax.Component where

import Prelude

import Data.Argonaut (Json, decodeJson, encodeJson, fromString, jsonParser)
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)
import Global (readFloat)
import Halogen (fork)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax (defaultRequest)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (Request(..))
import Network.HTTP.Affjax.Request as AXRequest
import Network.HTTP.Affjax.Response as AXResponse
import Network.HTTP.RequestHeader (RequestHeader(..))

foreign import _awaitSignIn :: Unit -> EffectFnAff String

awaitSignIn :: Aff String
awaitSignIn = (fromEffectFnAff <<< _awaitSignIn) unit

type State =
  { loading :: Boolean
  , apiBaseUri :: String
  , profile :: Profile
  , result :: Maybe String
  , idToken :: Maybe String
  }

type Profile =
  { name :: String
  , longitude :: String
  , latitude :: String
  , games :: String
  , age :: String
  , host :: Boolean
  }

type RequestProfile =
  { name :: String
  , longitude :: Number
  , latitude :: Number
  , games :: Array String
  , age :: Number
  , availableToHost :: Boolean
  }

data Query a
  = SetProfileName String a
  | SetProfileLongitude String a
  | SetProfileLatitude String a
  | SetProfileGames String a
  | SetProfileAge String a
  | SetProfileHost Boolean a
  | RegisterProfile a
  | UpdateProfile a
  | GetProfile a
  | AwaitSignIn a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action AwaitSignIn)
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState =
    { loading: false
    , apiBaseUri: "http://localhost:3000"
    , profile:
      { name: ""
      , age: ""
      , longitude: ""
      , latitude: ""
      , games: "[\"Settlers of Catan\", \"Risk\", \"Chess\"]"
      , host: false
      }
    , result: Nothing
    , idToken: Nothing
    }

  render :: forall m. State -> H.ComponentHTML Query () m
  render st =
    HH.div_ $
      [ HH.form_  $
          [ HH.h1_ [ HH.text "API BASE URI" ]
          , HH.label_
              [ HH.div_ [ HH.text "URL:" ]
              , HH.input
                  [ HP.value st.apiBaseUri
                  , HE.onValueInput (HE.input SetProfileName)
                  ]
              ]
          ]
      , HH.p_
          [ HH.div
              [ HP.class_ $ H.ClassName "g-signin2"
              , HP.attr (H.AttrName "data-onsuccess") "onSignIn"
              ]
              [ HH.text "Login" ]
          ]
      , HH.p_
          [ HH.text $ "IdToken: " <> show st.idToken ]
      , HH.form_  $
          [ HH.h1_ [ HH.text "Profile" ]
          , HH.label_
              [ HH.div_ [ HH.text "Name:" ]
              , HH.input
                  [ HP.value st.profile.name
                  , HE.onValueInput (HE.input SetProfileName)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Longitude:" ]
              , HH.input
                  [ HP.value st.profile.longitude
                  , HE.onValueInput (HE.input SetProfileLongitude)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Latitude:" ]
              , HH.input
                  [ HP.value st.profile.latitude
                  , HE.onValueInput (HE.input SetProfileLatitude)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Games:" ]
              , HH.input
                  [ HP.value st.profile.games
                  --[ HP.value $ maybe "" (stringify <<< encodeJson) st.profile.games
                  , HE.onValueInput (HE.input SetProfileGames)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Age:" ]
              , HH.input
                  [ HP.value st.profile.age
                  , HE.onValueInput (HE.input SetProfileAge)
                  ]
              ]
          , HH.label_
              [ HH.div_ [ HH.text "Host:" ]
              , HH.input
                  [ HP.type_ InputCheckbox
                  , HP.checked st.profile.host
                  , HE.onChecked (HE.input SetProfileHost)
                  ]
              ]
          , HH.p_
              [ HH.button
                  [ HP.disabled st.loading
                  , HP.type_ HP.ButtonButton
                  , HE.onClick (HE.input_ RegisterProfile)
                  ]
                  [ HH.text "Register" ]
              , HH.button
                  [ HP.disabled st.loading
                  , HP.type_ HP.ButtonButton
                  , HE.onClick (HE.input_ UpdateProfile)
                  ]
                  [ HH.text "Update" ]
              , HH.button
                  [ HP.disabled st.loading
                  , HP.type_ HP.ButtonButton
                  , HE.onClick (HE.input_ GetProfile)
                  ]
                  [ HH.text "Get" ]
              ]
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

  requestifyProfile :: Profile -> RequestProfile
  requestifyProfile p =
    { name: p.name
    , age: readFloat p.age
    , longitude: readFloat p.longitude
    , latitude: readFloat p.latitude
    , games: either (const []) identity $ decodeJson =<< jsonParser p.games
    , availableToHost: p.host
    }

  eval :: Query ~> H.HalogenM State Query () Void Aff
  eval = case _ of
    SetProfileName name a -> do
      H.modify_ (\state -> state { profile = state.profile { name = name } })
      pure a
    SetProfileLongitude longitude a -> do
      H.modify_ (\state -> state { profile = state.profile { longitude = longitude } })
      pure a
    SetProfileLatitude latitude a -> do
      H.modify_ (\state -> state { profile = state.profile { latitude = latitude } })
      pure a
    SetProfileGames games a -> do
      H.modify_ (\state -> state { profile = state.profile { games = games } })
      pure a
    SetProfileAge age a -> do
      H.modify_ (\state -> state { profile = state.profile { age = age } })
      pure a
    SetProfileHost host a -> do
      H.modify_ (\state -> state { profile = state.profile { host = host } })
      pure a
    RegisterProfile a -> do
      state <- H.get
      H.modify_ (_ { loading = true })
      response <- H.liftAff
                    $ AX.affjax
                        AXResponse.string
                        $ defaultRequest
                          { url = state.apiBaseUri <> "/register"
                          , content = Just $ AXRequest.json $ encodeJson $ requestifyProfile state.profile
                          , headers = maybe [] (pure <<< RequestHeader "Authorization" <<< ("Bearer: " <> _)) state.idToken
                          , method = Left POST
                          }
      H.modify_ (_ { loading = false, result = Just response.response })
      pure a
    UpdateProfile a -> do
      state <- H.get
      H.modify_ (_ { loading = true })
      response <- H.liftAff
                    $ AX.affjax
                        AXResponse.string
                        $ defaultRequest
                          { url = state.apiBaseUri <> "/register"
                          , content = Just $ AXRequest.json $ encodeJson $ requestifyProfile state.profile
                          , headers = maybe [] (pure <<< RequestHeader "Authorization" <<< ("Bearer: " <> _)) state.idToken
                          , method = Left PUT
                          }
      H.modify_ (_ { loading = false, result = Just response.response })
      pure a
    GetProfile a -> do
      state <- H.get
      H.modify_ (_ { loading = true })
      response <- H.liftAff
                    $ AX.affjax
                        AXResponse.string
                        $ defaultRequest
                          { url = state.apiBaseUri <> "/register"
                          , headers = maybe [] (pure <<< RequestHeader "Authorization" <<< ("Bearer: " <> _)) state.idToken
                          , method = Left GET
                          }
      H.modify_ (_ { loading = false, result = Just response.response })
      pure a

    AwaitSignIn a -> do
      _ <- fork do
        idToken <- liftAff awaitSignIn
        H.modify_ (\state -> state { idToken = Just idToken })
      pure a
