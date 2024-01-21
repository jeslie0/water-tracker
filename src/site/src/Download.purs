module Download (downloadString) where

import Prelude

import Effect (Effect)

foreign import downloadString :: { fileName:: String, blob:: String } -> Effect Unit
