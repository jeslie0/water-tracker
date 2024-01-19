{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent qualified as CC
import Data.Aeson
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status
import System.Environment qualified as Env
import WaiAppStatic.Types
import Yesod
import Yesod.Static
import Yesod.WebSockets

data DrinkLog = DrinkLog
  { timeMs :: Int,
    volumeMl :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON DrinkLog

instance FromJSON DrinkLog

type DrinkLogs = [DrinkLog]

data HelloWorld = HelloWorld
  { drinkLog :: CC.MVar DrinkLogs,
    getStatic :: Static
  }

-- / HomeR GET
-- "/" - This is the URL
-- "HomeR" This is a route. It needs a corresponding handler function
-- which is the lowercase method name, followed by the route name.
mkYesod
  "HelloWorld"
  [parseRoutes|
/drinksLog DrinksLogR GET POST
!/ StaticR Static getStatic
|]

instance Yesod HelloWorld

getDrinksLogR :: HandlerFor HelloWorld Value
getDrinksLogR = do
  model <- getYesod
  list <- liftIO $ CC.readMVar $ drinkLog model
  returnJson list

postDrinksLogR :: HandlerFor HelloWorld ()
postDrinksLogR = do
  result <- parseCheckJsonBody :: Handler (Result DrinkLog)
  case result of
    Error str -> liftIO $ print $ "Error! " <> str
    Success log -> do
      model <- getYesod
      liftIO $ CC.modifyMVar_ (drinkLog model) (\logs -> do print $ "Success!: " <> show (length logs); return $ log : logs)

-- HelloWorld {..} <- getYesod
-- liftIO . CC.modifyMVar_ visitorCount $ \n -> return $ n + 1
-- val <- liftIO $ CC.readMVar visitorCount
-- defaultLayout $ do
--   setTitle "Page Counter"
--   addStylesheet $ StaticR $ StaticRoute ["css", "patternfly", "patternfly.min.css"] []
--   toWidget
--     [hamlet|Number of visitors: #{val}|]

main :: IO ()
main = do
  htmlDir <- Maybe.fromMaybe "../../src/site/public" <$> Env.lookupEnv "FITNESS_MONAD_HTML_DIR"
  putStrLn $ "Yesod serving static files from \"" <> htmlDir <> "\"."

  Static settings <- staticDevel htmlDir
  let staticSettings =
        settings
          { ssRedirectToIndex = False,
            ssIndices = [unsafeToPiece "index.html"]
          }
  mvar <- CC.newMVar []

  warp
    3001
    HelloWorld
      { drinkLog = mvar,
        getStatic = Static staticSettings
      }
