{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent qualified as CC
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import HelloSub
import System.Environment qualified as Env
import Yesod
import Yesod.Static
import Yesod.WebSockets
import Network.HTTP.Types.Status
import WaiAppStatic.Types

data HelloWorld = HelloWorld
  { visitorCount :: CC.MVar Int,
    getHelloSub :: HelloSub,
    getStatic :: Static
  }

-- / HomeR GET
-- "/" - This is the URL
-- "HomeR" This is a route. It needs a corresponding handler function
-- which is the lowercase method name, followed by the route name.
mkYesod
  "HelloWorld"
  [parseRoutesNoCheck|
/counter CounterR GET
/socket WebSocketR GET
/subsite SubsiteR HelloSub getHelloSub
/ StaticR Static getStatic
|]



instance Yesod HelloWorld


getCounterR :: HandlerFor HelloWorld Html
getCounterR = do
  HelloWorld {..} <- getYesod
  liftIO . CC.modifyMVar_ visitorCount $ \n -> return $ n + 1
  val <- liftIO $ CC.readMVar visitorCount
  defaultLayout [whamlet|Number of visitors: #{val}|]


getWebSocketR :: HandlerFor HelloWorld Html
getWebSocketR = do
  webSockets socketT
  sendResponseStatus ok200 ("Test" :: T.Text)


socketT :: WebSocketsT Handler ()
socketT = do
  echo :: T.Text <- receiveData
  sendTextData echo
  socketT


main :: IO ()
main = do
  htmlDir <- Maybe.fromMaybe "../../src/site/public" <$> Env.lookupEnv "FITNESS_MONAD_HTML_DIR"
  putStrLn $ "Yesod serving static files from \"" <> htmlDir <> "\"."

  Static settings <- staticDevel htmlDir
  let staticSettings =
        settings { ssRedirectToIndex = False
                 , ssIndices = [unsafeToPiece "index.html"]
                 }
  mvar <- CC.newMVar 0

  warp
    3000
    HelloWorld
      { visitorCount = mvar,
        getHelloSub = HelloSub,
        getStatic = Static staticSettings
      }
