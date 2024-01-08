{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module FIT.DataRecord.RecordHeader where

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Get (Get)
import Data.Binary.Put qualified as Put
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

import Data.Bits

-- * Message type

-- Enum to represent the type of message to parse.
data MessageType =
  DefinitionMessageType
  | DataMessageType
  deriving (Eq, Show)

determineMessageType :: Word8 -> MessageType
determineMessageType word =
  if shiftR word 6 == 0
  then DataMessageType
  else DefinitionMessageType



-- * Header types

data HeaderType =
  NormalHeaderType
  | CompressedTimestampHeaderType
  deriving Show

determineHeaderType :: Word8 -> HeaderType
determineHeaderType word =
  if shiftR word 7 == 0
  then NormalHeaderType
  else CompressedTimestampHeaderType


-- * Normal Header

data NormalHeader =
  NormalHeader { messageType :: MessageType
               , localMessageType :: Word8
               } deriving Show



instance Binary NormalHeader where
  get =
    getNormalHeader <$> Get.getWord8

  put _ =
    error "Not implemented"

getNormalHeader :: Word8 -> NormalHeader
getNormalHeader word =
  let
    messageType =
      if shiftR word 6 .&. 1 == 0
      then DataMessageType
      else DefinitionMessageType

    localMessageType =
      fromIntegral $ word .&. 15
  in
    NormalHeader messageType localMessageType







-- * Compressed timestamp header


-- This type contains time offset information. It is only used for
-- data messages, only.
data CompressedTimestampHeader =
  CompressedTimestampHeader { localMessageType :: Word8
                            , timeOffsetS :: Word8
                            } deriving Show

instance Binary CompressedTimestampHeader where
  get =
    getCompressedTimestampHeader <$> Get.getWord8


  put _ =
    error "Not implemented"

getCompressedTimestampHeader :: Word8 -> CompressedTimestampHeader
getCompressedTimestampHeader word =
  let
    localMessageType =
      shiftR word 5 .&. 3

    timeOffsetS =
      word .&. 31
  in
    CompressedTimestampHeader localMessageType timeOffsetS


-- Both Definition and Data Messages contain a one byte header, which
-- distinguishes the type of header and the message type. There are
-- two types of header - a normal header and a compressed timestamp
-- header.
newtype RecordHeader =
  RecordHeader (Either NormalHeader CompressedTimestampHeader)
  deriving Show

getRecordHeader :: Word8 -> RecordHeader
getRecordHeader word =
  let
    msb = shiftR word 7
  in
    RecordHeader $
    if msb == 0
    then Left $ getNormalHeader word
    else Right $ getCompressedTimestampHeader word

instance Binary RecordHeader where
  get =
    getRecordHeader <$> Get.getWord8

  put _ =
    error "Not defined"
