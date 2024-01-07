{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

module LinearFFI where

import ArithFFI qualified
import Control.Functor.Linear qualified as Control
import Data.Text
import Data.Text.IO qualified as TIO
import Data.Unrestricted.Linear qualified as UR
import Debug.Trace (trace)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, freeHaskellFunPtr)
import GHC.Conc.IO (threadDelay)
import Prelude.Linear qualified as L
import Prelude.Linear qualified as PL
import System.IO qualified as System
import System.IO.Linear qualified as Linear
import System.IO.Resource.Linear qualified as Resource
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import System.Posix (sleep)

type RIO = Resource.RIO

mkFun :: (Int -> Int) -> RIO (Resource.Resource (FunPtr (CInt -> CInt)))
mkFun f =
  Resource.unsafeAcquire
    (Linear.fromSystemIOU . ArithFFI.mkFun $ fromIntegral . f . fromIntegral)
    (\funPtr -> Linear.fromSystemIO L.$ freeHaskellFunPtr funPtr)

foop :: (Int -> Int) -> Int -> RIO (L.Ur Int)
foop f n = Control.do
  funPtrRes <- mkFun f
  (result, res) <-
    Resource.unsafeFromSystemIOResource
      (\funPtr -> return . fromIntegral $! ArithFFI.c_fun_ptr_test funPtr (fromIntegral n))
      funPtrRes
  Resource.release res
  Control.return result

bar :: (Int -> Int) -> Int -> Int
bar f n =
  unsafePerformIO . Resource.run $ foop f n
