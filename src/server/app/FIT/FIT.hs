{-# LANGUAGE RecordWildCards #-}
module FIT.FIT where

-- This module contains a Binary instance for the FIT data type. The
-- data type represents a "FIT file", as specified by the FIT
-- Protocol.

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Get (Get)
import Data.Binary.Put qualified as Put
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

import FIT.FileHeader
import FIT.DataRecord.DataRecord


-- A FIT file consists of a file header, a sequence of data records as
-- its body, and a two byte CRC at the end of the file.
data FitFile =
  FitFile { fileHeader :: FileHeader
          , fileDataRecords :: [DataRecord]
          , fileCRC :: Word16
          } deriving Show

instance Binary FitFile where
  put = error "Not implemented"

  get = do
    fileHeader <- get
    fileDataRecords <- getDataRecords (dataSize fileHeader)
    FitFile fileHeader fileDataRecords <$> Get.getWord16le


