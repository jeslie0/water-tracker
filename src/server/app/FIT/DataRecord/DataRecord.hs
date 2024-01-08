{-# LANGUAGE DuplicateRecordFields #-}
module FIT.DataRecord.DataRecord where

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Get (Get)
import Data.Binary.Put qualified as Put
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

import FIT.DataRecord.RecordHeader
import FIT.DataRecord.DefinitionMessage
import FIT.DataRecord.DataMessage

import Debug.Trace

newtype DataRecord =
  DataRecord (Either DefinitionMessage DataMessage)
  deriving Show

instance Binary DataRecord where
  put = error "Not defined"

  get = do
    firstByte <- Get.getWord8
    let recordHeader@(RecordHeader eitherHeader) =
          getRecordHeader firstByte

        recordBody =
          case eitherHeader of
            Left normalHeader -> do
              case messageType normalHeader of
                DefinitionMessageType ->
                  getDefinitionMessage recordHeader

                DataMessageType -> getDataRecords


            Right compressedTimeStampHeader ->
              DataMessage

    return $ DataRecord (Right recordBody)







-- To get all the data records, we need to know the number of bytes
-- they take up in the file. This is given to us in the file header,
-- which is the Word32 that is passed into this function.
getDataRecords :: Word32 -> Get [DataRecord]
getDataRecords size = getDataRecordsHelper [] 0
  where
   getDataRecordsHelper :: [DataRecord] -> Word32 -> Get [DataRecord]
   getDataRecordsHelper acc n
     | n == size = return acc
     | otherwise = do
        dataRecord <- get
        amountRead <- Get.bytesRead
        getDataRecordsHelper (dataRecord : acc) (fromIntegral amountRead)
