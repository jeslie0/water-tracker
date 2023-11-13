{-# LANGUAGE OverloadedRecordDot #-}
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
  [parseRoutes|
/ HomeR GET
/subsite SubsiteR HelloSub getHelloSub
/static StaticR Static getStatic
|]

instance Yesod HelloWorld

getHomeR :: HandlerFor HelloWorld Html
getHomeR = do
  HelloWorld {..} <- getYesod
  liftIO . CC.modifyMVar_ visitorCount $ \n -> return $ n + 1
  val <- liftIO $ CC.readMVar visitorCount
  defaultLayout [whamlet|Hello, test! #{val}|]

-- getHomeR = do
--   liftIO $ static ""

main :: IO ()
main = do
  htmlDir <- Maybe.fromMaybe "../../src/site/public" <$> Env.lookupEnv "FITNESS_MONAD_HTML_DIR"
  putStrLn $ "Yesod serving static files from \"" <> htmlDir <> "\"."

  Static settings <- static htmlDir
  mvar <- CC.newMVar 0

  warp
    3000
    HelloWorld
      { visitorCount = mvar,
        getHelloSub = HelloSub,
        getStatic = Static settings
      }
