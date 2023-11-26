{-# LANGUAGE OverloadedRecordDot #-}
module FIT.FileHeader where

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Put qualified as Put
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

data FileHeader = FileHeader
  { protocolVersion :: !Word8,
    profileVersion :: !Word16,
    dataSize :: !Word32,
    crc :: !(Either Word8 Word16)
  } deriving Show

instance Binary FileHeader where
  put fileheader =
    case crc fileheader of
      Left crcByte -> do
        Put.putWord8 12
        middleOfHeader
        Put.putWord8 crcByte

      Right crcBytes -> do
        Put.putWord8 14
        middleOfHeader
        Put.putWord16le crcBytes
    where
      middleOfHeader = do
        Put.putWord8 fileheader.protocolVersion
        Put.putWord16le fileheader.profileVersion
        Put.putWord32le fileheader.dataSize
        Put.putWord8 _period
        Put.putWord8 _F
        Put.putWord8 _I
        Put.putWord8 _T

  get = do
    headerSizeBytes <- Get.getWord8
    if headerSizeBytes == 12 || headerSizeBytes == 14 then return () else fail "Header size is invalid."
    protocolByte <- Get.getWord8
    profileVersionBytes <- Get.getWord16le
    dataSizeBytes <- Get.getWord32le

    Get.getWord8 >>= \dotByte -> if dotByte == _period then return () else fail "Period not in correct place in file header"
    Get.getWord8 >>= \fByte -> if fByte == _F then return () else fail "\"F\" not in correct place in file header"
    Get.getWord8 >>= \iByte -> if iByte == _I then return () else fail "\"I\" not in correct place in file header"
    Get.getWord8 >>= \tByte -> if tByte == _T then return () else fail "\"T\" not in correct place in file header"

    crcVal <-
          if headerSizeBytes == 12
          then Left <$> Get.getWord8
          else if headerSizeBytes == 14
          then Right <$> Get.getWord16le
          else fail "Header size is not 12 or 14!"

    return $ FileHeader protocolByte profileVersionBytes dataSizeBytes crcVal
