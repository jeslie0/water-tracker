module Properties
       where

import Prelude
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

newtype PFProp r = PFProp (Record r -> Record r )

buildOptions :: forall r . Array (PFProp r) -> Record r -> Record r
buildOptions opArray init =
  ST.run do
    curOpRef <- STRef.new init
    ST.foreach opArray $ \ (PFProp func) -> do
      _ <- STRef.modify func curOpRef
      pure unit
    STRef.read curOpRef
