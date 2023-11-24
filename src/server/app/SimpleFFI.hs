{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
module SimpleFFI where

import Foreign ()
import Foreign.C.Types

foreign import capi unsafe "SimpleFFI.h printlol" c_print :: IO ()

foreign import capi unsafe "math.h sin" c_sin :: CDouble -> CDouble

fastSin :: Double -> Double
fastSin = realToFrac . c_sin . realToFrac

main :: IO ()
main = do
  print $ fastSin $ 3.14 + 3.14/2
  c_print

hs = main
