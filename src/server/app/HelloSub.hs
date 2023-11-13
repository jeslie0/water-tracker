{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module HelloSub
  ( module HelloSub.Data
  , module HelloSub
  ) where

import HelloSub.Data
import Yesod

getSubHomeR :: Yesod master => SubHandlerFor HelloSub master Html
getSubHomeR =
  liftHandler $ defaultLayout [whamlet|Welcome to the jungle|]

instance Yesod master => YesodSubDispatch HelloSub master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesHelloSub)
