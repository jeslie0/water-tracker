module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)


import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location (host)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render: HH.lazy render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just GetURL
      }
    }


-- | MODEL

type Model =
  { currentURL :: Maybe String }

initialState :: forall input. input -> Model
initialState _ =
  { currentURL: Nothing }

-- | UPDATE

data Action =
  GetURL

handleAction :: forall output m . MonadEffect m => Action -> H.HalogenM Model Action () output m Unit
handleAction action =
  case action of
    GetURL -> do
      l <- H.liftEffect $ window >>= location >>= host
      H.modify_ $ \state -> state { currentURL = Just l }



-- | VIEW

render :: forall m. Model -> H.ComponentHTML Action () m
render model =
  HH.div_
  [
    HH.div_
      [ HH.text "Hello, world! Click "
      , HH.a [HP.href "/counter"] [ HH.text "here"]
      , HH.text $ " to get a page counter."
      ]
  , HH.div_
    [ HH.text $ "Initiate a websocket on ws://" <> fromMaybe "" model.currentURL <> "/socket to get an echo server."]
  , HH.div_
    [ HH.text "Finally, go to "
      , HH.a [HP.href "/subsite"] [ HH.text "here"]
      , HH.text " to visit the subsite."
      ]
  , HH.div_
    [ HH.text "Site made with PureScript & Halogen on the frontend, and Haskell and Yesod on the backend."
    ]
  ]
