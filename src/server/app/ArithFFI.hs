{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module ArithFFI where

import Control.Monad.Trans.Resource (allocate, runResourceT)
import Control.Exception (bracket, mask_, finally)
import Control.Monad
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, castFunPtr)
import GHC.Conc.IO (threadDelay)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import System.Posix (sleep)

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
newArith =
  mask_ c_arithNew >>= newForeignPtr c_arithDelete

mainFFI :: IO ()
mainFFI =
  newArith >>= \arith -> withForeignPtr arith $ \ptr -> do
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


-- * passing functions to C to call
-- The result of the c_fun_ptr_test must be strict, otherwise the
-- release command will be called too early.
foreign import ccall safe "arith_capi.h fun_ptr_test" c_fun_ptr_test :: FunPtr (CInt -> CInt) -> CInt -> CInt

foreign import ccall "wrapper" mkFun :: (CInt -> CInt) -> IO (FunPtr (CInt -> CInt))

bar :: (Int -> Int) -> Int -> Int
bar f n = unsafePerformIO do
  funptr <- mkFun (fromIntegral . f . fromIntegral)
  let !m = fromIntegral $ c_fun_ptr_test funptr (fromIntegral n)
  freeHaskellFunPtr funptr
  return m


foop :: (Int -> Int) -> Int -> Int
foop f n = unsafePerformIO $
  bracket
    (mkFun (fromIntegral . f . fromIntegral))
    freeHaskellFunPtr
    (\funptr -> return . fromIntegral $! c_fun_ptr_test funptr (fromIntegral n))


baz :: (Int -> Int) -> Int -> Int
baz f n = unsafePerformIO . runResourceT $ do
  (_, funPtr) <- allocate (mkFun (fromIntegral . f . fromIntegral)) freeHaskellFunPtr
  return $! fromIntegral . c_fun_ptr_test funPtr . fromIntegral $ n
