{-# LANGUAGE DuplicateRecordFields #-}
module FIT.DataRecord.DataRecord
  (DataRecord(..)
  )where

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Put qualified as Put
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

import FIT.DataRecord.HeaderByte


data DataRecord =
  DataRecord (Either DefinitionMessage DataMessage)
  DefinitionMessage { recordHeader :: Hea  }
  | DataMessage { recordHeader :: Word8 }
