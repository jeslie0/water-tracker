{-# LANGUAGE ForeignFunctionInterface #-}
module ArithFFI where

import Control.Exception (mask_)
import Foreign.Ptr ( FunPtr, Ptr )
import Foreign.C.Types ( CInt(..) )
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import System.Posix (sleep)
import GHC.Conc.IO (threadDelay)
import System.Mem (performGC)

data Arith

foreign import ccall unsafe "arith_capi.h arith_new" c_arithNew :: IO (Ptr Arith)
foreign import ccall unsafe "arith_capi.h &arith_delete" c_arithDelete :: FunPtr (Ptr Arith -> IO ())
foreign import ccall unsafe "arith_capi.h arith_add" c_arithAdd :: Ptr Arith -> CInt -> CInt -> CInt
foreign import ccall unsafe "arith_capi.h arith_sub" c_arithSub :: Ptr Arith -> CInt -> CInt -> CInt
foreign import ccall unsafe "arith_capi.h arith_mult" c_arithMult :: Ptr Arith -> CInt -> CInt -> CInt
foreign import ccall unsafe "arith_capi.h arith_div" c_arithDiv :: Ptr Arith -> CInt -> CInt -> CInt

-- | Create a new foreign object that will be cleaned after it's not in use
-- anymore. It also uses mask_ in case the pointer leaks if an exception happens.
newArith :: IO (ForeignPtr Arith)
newArith = do
  arithPtr <- mask_ c_arithNew
  newForeignPtr c_arithDelete arithPtr

mainFFI :: IO ()
mainFFI = newArith >>= \arith -> withForeignPtr arith $ \ptr -> do
  -- Foreign object is now unwrapped to a foreign pointer which you can use in
  -- any FFI function you described above.
  forM_ [c_arithAdd ptr 1 1, c_arithSub ptr 1 1, c_arithMult ptr 2 2, c_arithDiv ptr 2 2] print


myAdd :: Int -> Int -> Int
myAdd x y = unsafePerformIO $ do
  arithPtr <- newArith
  withForeignPtr arithPtr $ \ptr -> return . fromIntegral $ c_arithAdd ptr (fromIntegral x) (fromIntegral y)


newtype Foo = Foo (ForeignPtr Arith)

mkFoo :: Foo
mkFoo = Foo . unsafePerformIO $ newArith

fooAdd :: Foo -> Int -> Int -> Int
fooAdd (Foo fooPtr) n m = unsafePerformIO $ do
  withForeignPtr fooPtr $ \ptr -> return . fromIntegral $ c_arithAdd ptr (fromIntegral n) (fromIntegral m)

fooIO :: IO ()
fooIO = do
  -- print $ myAdd 1 2
  print $ fooAdd mkFoo 1 5

addIO :: IO ()
addIO =
  print $ myAdd 1 2
