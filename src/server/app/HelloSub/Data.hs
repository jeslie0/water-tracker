{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module HelloSub.Data where

import           Yesod

data HelloSub = HelloSub

mkYesodSubData "HelloSub" [parseRoutes|
/ SubHomeR GET
|]
