{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Concurrent
import qualified Data.Text          as T
import           HelloSub
import           Yesod
import           Yesod.Static

data HelloWorld =
  HelloWorld { visitorCount :: MVar Int
             , getHelloSub :: HelloSub
             , getStatic :: Static
             }

-- / HomeR GET
-- "/" - This is the URL
-- "HomeR" This is a route. It needs a corresponding handler function
-- which is the lowercase method name, followed by the route name.
mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/subsite SubsiteR HelloSub getHelloSub
/static StaticR Static getStatic
|]

instance Yesod HelloWorld

getHomeR :: HandlerFor HelloWorld Html
getHomeR = do
  HelloWorld {..} <- getYesod
  liftIO . modifyMVar_ visitorCount $ \n -> return $ n + 1
  val <- liftIO $ readMVar visitorCount
  defaultLayout [whamlet|Hello, test! #{val}|]


-- getHomeR = do
--   liftIO $ static ""



main :: IO ()
main = do
  mvar <- newMVar 0
  staticSite <- static "../../src/site/public/"
  warp 3000 HelloWorld
    { visitorCount = mvar
    , getHelloSub = HelloSub
    , getStatic = staticSite
    }
