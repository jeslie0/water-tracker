module Main where

import Prelude

import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array ((:))
import Data.Array as Array
import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (format, FormatterCommand(..))
import Data.Int (fromString)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Effect.Now as Now
import Fetch (Method(..))
import Fetch as Fetch
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Patternfly.Button as Button
import Halogen.Patternfly.Card as Card
import Halogen.Patternfly.Gallery as Gallery
import Halogen.Patternfly.Icons.Moon as Moon
import Halogen.Patternfly.Icons.Sun as Sun
import Halogen.Patternfly.Icons.EllipsisV as EllipsisV
import Halogen.Patternfly.Masthead as Masthead
import Halogen.Patternfly.Page as Page
import Halogen.Patternfly.Properties as HPP
import Halogen.Patternfly.Table as Table
import Halogen.Patternfly.Text as Text
import Halogen.Patternfly.ToggleGroup as ToggleGroup
import Halogen.Patternfly.Toolbar as Toolbar
import Halogen.VDom.Driver (runUI)
import MediaQuery (matchMedia, matches)
import Web.DOM.Document (getElementsByTagName)
import Web.DOM.Element (removeAttribute, setClassName)
import Web.DOM.HTMLCollection (item)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Location (href)
import Web.HTML.Window (document, location)
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
  , currentVolumeMl :: Int
  , theme :: Theme
  , page :: Page
  , configWindowOpen :: Boolean
  }

initialState :: forall input. input -> Model
initialState _ =
  { currentURL: Nothing
  , drinkLog: []
  , currentVolumeMl: 500
  , theme: Light
  , page: Today
  , configWindowOpen: false
  }

-- | UPDATE

data Page
  = Today
  | Logs
  | Charts

instance Show Page where
  show Today = "Today"
  show Logs = "Logs"
  show Charts = "Charts"

derive instance eqPage :: Eq Page

data Theme
  = Light
  | Dark

derive instance eqTheme :: Eq Theme

data Action
  = Init
  | LogDrink
  | UpdateCurrentVolume String
  | ChangeTheme Theme
  | ChangePage Page
  | ToggleConfigWindow


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
      url <- H.liftEffect $ window >>= location >>= href
      -- let url = "http://localhost:3001/"
      logsE <- H.liftAff <<< Aff.attempt <<< fetchLogs $ url
      mediaQueryList <- H.liftEffect $ window >>= \w -> matchMedia w "(prefers-color-scheme: dark)"
      let
        darkMode = matches mediaQueryList
        theme = if darkMode then Dark else Light
      case logsE of
        Left _ -> do
          H.modify_ $ _ { currentURL = Just url, theme = theme }
          H.liftEffect $ changeTheme theme

        Right logs -> do
          H.modify_ $ _ { currentURL = Just url, drinkLog = logs, theme = theme }
          H.liftEffect $ changeTheme theme

    LogDrink -> do
      timeNowMs <- H.liftEffect Now.now
      H.modify_ $ \model -> model { drinkLog = Array.cons { timeMs: timeNowMs, volumeMl: model.currentVolumeMl } model.drinkLog }
      { currentURL, currentVolumeMl } <- H.get
      case currentURL of
        Nothing -> pure unit
        Just url -> do
          H.liftAff do
            _ <- Fetch.fetch (url <> "drinksLog")
              { method: POST
              , headers: { "Content-Type": "application/json" }
              , body: JSON.writeJSON ({ timeMs: timeNowMs, volumeMl: currentVolumeMl })
              }
            pure unit

    UpdateCurrentVolume str ->
      case fromString str of
        Nothing -> pure unit
        Just n ->
          H.modify_ $ _ { currentVolumeMl = n }

    ChangeTheme theme -> do
      H.modify_ $ \model -> model { theme = theme }
      H.liftEffect $ changeTheme theme

    ChangePage page ->
      H.modify_ $ \model -> model { page = page }

    ToggleConfigWindow ->
      H.modify_ $ \model -> model { configWindowOpen = not model.configWindowOpen }

changeTheme :: Theme -> Effect Unit
changeTheme theme = do
  doc <- window >>= document
  htmlCollection <- getElementsByTagName "html" $ toDocument doc
  mRoot <- item 0 htmlCollection
  case mRoot of
    Nothing -> pure unit
    Just rootElem -> do
      case theme of
        Dark -> setClassName "pf-v5-theme-dark" rootElem
        Light -> removeAttribute "class" rootElem

-- | VIEW

render :: forall m. Model -> H.ComponentHTML Action () m
render model =
  Page.page [ HPP.header $ header model ] [] $
    case model.page of
      Today -> todayPage model
      Logs -> logsPage model
      Charts -> chartsPage model

header :: forall i m r. { page :: Page, theme :: Theme | r } -> H.ComponentHTML Action i m
header { page, theme } =
  Masthead.masthead [] []
    [ Masthead.mastheadMain [] []
        [ HH.text "Water Tracker"
        ]
    , Masthead.mastheadContent [] []
        [ Toolbar.toolbar [] []
            [ Toolbar.toolbarContent [] []
                [ Toolbar.toolbarGroup [] [] pagesToolbarItems
                , Toolbar.toolbarGroup [ HPP.align HPP.Right ] [] toggleAndOptions
                ]
            ]
        ]
    ]
  where
  pagesToolbarItems =
    Toolbar.separator : ([ Today, Logs, Charts ] <#> \pg ->
      Toolbar.toolbarItem [] []
        [ Button.button
            [ HPP.useVariant Button.Plain ]
            [ HE.onClick $ \_ -> ChangePage pg ]
            [ if page == pg then HH.a_  [HH.text <<< show $ pg] else HH.text <<< show $ pg ]
        ])

  toggleAndOptions =
     [ Toolbar.toolbarItem [] []
        [ ToggleGroup.toggleGroup [] [ HP.class_ $ H.ClassName "pf-m-align-right" ]
          [ ToggleGroup.toggleGroupItem
            [ HPP.isSelected (theme == Light)
            , HPP.onClick $ \_ -> ChangeTheme Light
            , HPP.content $ HH.span
              [ HP.class_ $ H.ClassName "pf-v5-c-toggle-group__icon" ]
              [ HH.span
                [ HP.classes [ H.ClassName "pf-v5-c-icon", H.ClassName "pf-m-md" ]
                ]
                [ HH.span [ HP.class_ $ H.ClassName "pf-v5-c-icon__content" ] [ Sun.sun ] ]
              ]
            ]
          , ToggleGroup.toggleGroupItem
            [ HPP.isSelected (theme == Dark)
            , HPP.onClick $ \_ -> ChangeTheme Dark
            , HPP.content $ HH.span
              [ HP.class_ $ H.ClassName "pf-v5-c-toggle-group__icon" ]
              [ HH.span
                [ HP.classes [ H.ClassName "pf-v5-c-icon", H.ClassName "pf-m-md" ]
                ]
                [ HH.span [ HP.class_ $ H.ClassName "pf-v5-c-icon__content" ] [ Moon.moon ] ]
              ]
            ]
          ]
        ]
     , Toolbar.separator
     , Toolbar.toolbarItem [] []
       [ Button.button
         [ HPP.useVariant Button.Plain ]
         [ HE.onClick $ \_ -> ToggleConfigWindow]
         [ EllipsisV.ellipsisV ]
       ]
       ]


todayPage :: forall m. Model -> Array (H.ComponentHTML Action () m)
todayPage model =
  [ Card.card [ HPP.isFullHeight true ] [ HP.style "height: 100%;" ]
      [ Card.body [ HPP.isFilled true ] []
          [ HH.div_ $ waterImage model : inputML model
          , logDrinkButton
          ]
      ]
  , waterTable model
  ]

waterImage :: forall m r. { currentURL :: Maybe String | r } -> H.ComponentHTML Action () m
waterImage { currentURL } =
  case currentURL of
    Nothing ->
      HH.div_ []

    Just url ->
      HH.img
        [ HP.src $ url <> "/water.png"
        ]

inputML :: forall m r. { currentVolumeMl :: Int | r } -> Array (H.ComponentHTML Action () m)
inputML { currentVolumeMl } =
  [ HH.div
      [ HP.style "text-align: center;" ]
      [ HH.input
          [ HP.type_ InputNumber
          , HP.value <<< show $ currentVolumeMl
          , HE.onValueChange UpdateCurrentVolume
          , HP.id "volume"
          , HP.attr (H.AttrName "size") "5"
          ]
      , HH.text " (ml)"
      ]
  ]

logDrinkButton :: forall m. H.ComponentHTML Action () m
logDrinkButton =
  Button.button
    []
    [ HE.onClick $ \_ -> LogDrink
    , HP.style "display: block; margin-left: auto; margin-right: auto;"
    ]
    [ HH.text "Log drink" ]

waterTable :: forall m r. { drinkLog :: Array DrinkLog | r } -> H.ComponentHTML Action () m
waterTable { drinkLog } =
  Card.card [] []
    [ Card.body [] []
        [ Table.table [ HPP.isCompact true ] []
            [ Table.thead [] []
                [ Table.tr [] []
                    [ Table.th [] [] [ HH.text "Time" ]
                    , Table.th [] [] [ HH.text "Amount (ml)" ]
                    ]
                ]
            , Table.tbody [] [] $ drinkLog <#>
                \log -> Table.tr [] []
                  [ Table.td [] []
                      [ HH.text
                          <<< format
                            ( List.fromFoldable
                                [ Hours24
                                , Placeholder ":"
                                , MinutesTwoDigits
                                ]
                            )
                          <<< toDateTime $ log.timeMs
                      ]
                  , Table.td [] [] [ HH.text $ show log.volumeMl ]
                  ]
            ]
        ]
    ]

logsPage :: forall m r. { drinkLog :: Array DrinkLog | r } -> Array (H.ComponentHTML Action () m)
logsPage { drinkLog } =
  [ Card.card [ HPP.isFullHeight true ] [ HP.style "height: 100%;" ]
      [ Card.body [ HPP.isFilled true ] []
          [ presentLogs <<< organiseDates $ drinkLog ]
      ]
  ]

-- | Get the logs for a certain date. Return both the logs that
-- | satisfy that and don't.
getDateLogs :: Date -> Array DrinkLog -> { correctDate :: Array DrinkLog, incorrectDate :: Array DrinkLog }
getDateLogs date logs =
  let
    { yes, no } = Array.partition
      ( \log ->
          let
            logDate = DateTime.date <<< toDateTime $ log.timeMs
          in
            date == logDate
      )
      logs
  in
    { correctDate: yes
    , incorrectDate: no
    }

-- | Group the drink log array by date.
organiseDates :: Array DrinkLog -> Array { datetime :: DateTime, logs :: Array DrinkLog }
organiseDates arr =
  case Array.uncons arr of
    Nothing -> []
    Just { head, tail } ->
      let
        date =
          DateTime.date <<< toDateTime $ head.timeMs

        { correctDate, incorrectDate } =
          getDateLogs date tail
      in
        { datetime: toDateTime head.timeMs, logs: correctDate } : organiseDates incorrectDate

findToday :: forall a. Array { date :: Date | a } -> Effect (Maybe { date :: Date | a })
findToday arr = do
  Now.nowDate <#> (\nowDate -> Array.find (\log -> log.date == nowDate) arr)

presentLogs :: forall m. Array { datetime :: DateTime, logs :: Array DrinkLog } -> H.ComponentHTML Action () m
presentLogs arr =
  Gallery.gallery [ HPP.hasGutter true ] [] $
    arr <#> \ { datetime, logs } ->
      Text.textContent [] []
        [ Card.card [] []
            [ Card.title HH.h3 $ format
                ( List.fromFoldable
                    [ YearFull
                    , Placeholder ", "
                    , MonthFull
                    , Placeholder " "
                    , DayOfMonth
                    ]
                )
                datetime
            , waterTable { drinkLog: logs }
            ]
        ]

chartsPage :: forall m . Model -> Array (H.ComponentHTML Action () m)
chartsPage model =
  [ HH.div [] [] ]
