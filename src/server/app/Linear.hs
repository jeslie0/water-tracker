{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Linear where
import System.IO qualified as System
import System.IO.Linear qualified as Linear
import System.IO.Resource.Linear qualified as Resource
import Prelude.Linear qualified as L
import qualified Control.Functor.Linear as Control
import Data.Text
import Data.Text.IO qualified as TIO
import Debug.Trace (trace)
import Prelude.Linear qualified as PL


type RIO = Resource.RIO
type Handle = Resource.Resource System.Handle
type IOMode = System.IOMode

myOpenFile :: FilePath -> IOMode -> RIO Handle
myOpenFile path mode =
  Resource.unsafeAcquire
    (Linear.fromSystemIOU $ System.openFile path mode)
    (\h -> trace "Releasing!" $ Linear.fromSystemIO L.$ System.hClose h)

myhGetLine :: Handle %1 -> RIO (L.Ur Text, Handle)
myhGetLine = Resource.unsafeFromSystemIOResource TIO.hGetLine

linearGetFirstLine :: FilePath -> RIO (L.Ur Text)
linearGetFirstLine path = Control.do
  handle <- myOpenFile path System.ReadMode
  (text, newHandle) <- myhGetLine handle
  Resource.release newHandle
  Control.return text

main :: System.IO ()
main = do
 text <- Resource.run $ linearGetFirstLine "LICENSE"
 print text

data Either a b = Left a | Right b

foo :: Linear.Either Int Int %1 -> Int
foo (Linear.Left n) = 2 PL.* n
foo (Linear.Right n) = n

data UL a b where
  UL :: a -> b %1 -> UL a b

-- Using linear for handling releasing of foreign pointers
