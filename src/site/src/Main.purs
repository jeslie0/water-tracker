module Main where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

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
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }


-- | MODEL

type Model =
  {}

initialState :: forall input. input -> Model
initialState _ = {}

-- | UPDATE

data Action
handleAction :: forall output m . MonadEffect m => Action -> H.HalogenM Model Action () output m Unit
handleAction _ = pure unit

-- | VIEW

render :: forall m. Model -> H.ComponentHTML Action () m
render _ =
  HH.div []
  [ HH.text "Hello, world!" ]
