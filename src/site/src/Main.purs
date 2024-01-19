module Main where

import Prelude

import Data.Array as Array
import Data.DateTime (Millisecond, date)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (format, FormatterCommand(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Console as Console
import Effect.Now as Now
import Fetch (Method(..))
import Fetch as Fetch
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Location (href)
import Web.HTML.Window (location)
import Yoga.JSON (E)
import Yoga.JSON as JSON

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render: HH.lazy render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

-- | MODEL

type DrinkLog =
  { timeMs :: Instant
  , volumeMl :: Int
  }

type Model =
  { currentURL :: Maybe String
  , drinkLog :: Array DrinkLog
  }

initialState :: forall input. input -> Model
initialState _ =
  { currentURL: Nothing
  , drinkLog: []
  }

-- | UPDATE

data Action
  = Init
  | LogDrink

fetchLogs :: String -> Aff (Array DrinkLog)
fetchLogs baseUrl = do
  let url = baseUrl <> "drinksLog"
  { json } <- Fetch.fetch url { headers: { "Accept": "application/json" } }
  jsonContent <- json
  let (deserialisedM :: E (Maybe (Array DrinkLog))) = JSON.read jsonContent
  case deserialisedM of
    Left _ -> pure []
    Right Nothing -> do
      H.liftEffect $ Console.error "Could not deserialise logs."
      pure []
    Right (Just deserialised) -> do
      pure deserialised

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM Model Action () output m Unit
handleAction action =
  case action of
    Init -> do
      -- url <- H.liftEffect $ window >>= location >>= href
      let url = "http://localhost:3001/"
      logsE <- H.liftAff <<< Aff.attempt <<< fetchLogs $ url
      case logsE of
        Left _ ->
          H.modify_ $ _ { currentURL = Just url }

        Right logs ->
          H.modify_ $ _ { currentURL = Just url, drinkLog = logs }

    LogDrink -> do
      timeNowMs <- H.liftEffect Now.now
      H.modify_ $ \model -> model { drinkLog = Array.cons { timeMs: timeNowMs, volumeMl: 500 } model.drinkLog }
      { currentURL } <- H.get
      case currentURL of
        Nothing -> pure unit
        Just url -> do
          H.liftAff do
            _ <- Fetch.fetch (url <> "drinksLog")
              { method: POST
              , headers: { "Content-Type": "application/json" }
              , body: JSON.writeJSON ({ timeMs: timeNowMs, volumeMl: 500 })
              }
            pure unit

-- | VIEW

render :: forall m. Model -> H.ComponentHTML Action () m
render model =
  HH.div
    [ HP.style "margin: auto; width: 95%;" ]
    [ HH.div
        [ HP.class_ $ H.ClassName "centre" ]
        ( case model.currentURL of
            Nothing -> []
            Just url ->
              [ HH.img
                  [ HP.src $ url <> "/water.png"
                  , HP.class_ $ H.ClassName "centre"
                  ]
              ]
            <>
              [ HH.button
                  [ HP.class_ $ H.ClassName "centre"
                  , HE.onClick $ \_ -> LogDrink
                  ]
                  [ HH.text "Log drink" ]
              ]
        )
    , HH.table
        [ HP.class_ $ H.ClassName "centre" ]
        $
          [ HH.tr_
              [ HH.th [] [ HH.text "Time" ]
              , HH.th_ [ HH.text "Amount (ml)" ]
              ]
          ] <>
            ( model.drinkLog
                <#>
                  ( \log -> HH.tr_
                      [ HH.td_
                          [ HH.text
                              <<< format
                                ( List.fromFoldable
                                    [ YearTwoDigits
                                    , Placeholder ", "
                                    , MonthShort
                                    , Placeholder " "
                                    , DayOfMonth
                                    , Placeholder " - "
                                    , Hours24
                                    , Placeholder ":"
                                    , MinutesTwoDigits
                                    ]
                                )
                              <<< toDateTime $ log.timeMs
                          ]
                      , HH.td_ [ HH.text $ show log.volumeMl ]
                      ]
                  )
            )

    ]
