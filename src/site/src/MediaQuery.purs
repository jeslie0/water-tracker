module MediaQuery where

import Effect (Effect)
import Web.HTML.Window (Window)

foreign import data MediaQueryList :: Type

foreign import matchMedia :: Window -> String -> Effect MediaQueryList

foreign import matches :: MediaQueryList -> Boolean
