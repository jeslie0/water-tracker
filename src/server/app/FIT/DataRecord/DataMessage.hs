{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module FIT.DataRecord.DataMessage where

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Get (Get)
import Data.Binary.Put qualified as Put
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

import FIT.DataRecord.DefinitionMessage
import FIT.DataRecord.RecordHeader

data DataMessage =
  DataMessage { header :: RecordHeader }
  deriving Show


getDataMessage :: DefinitionMessage -> Get DataMessage
getDataMessage (DefinitionMessage {..}) = do
  headerByte <- Get.getWord8
  let header = getRecordHeader headerByte
  return $ DataMessage header



instance Binary DataMessage where
  put = error "Not implemented"

  get = error "Not implemented"
