{-# LANGUAGE OverloadedRecordDot #-}
module FIT.DataRecord.DefinitionMessage where

import Data.Binary (Binary (..))
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Get
import Data.Binary.Get (Get)
import Data.Binary.Put qualified as Put
import Data.Memory.Endian (Endianness(..))
import Data.Word
import Data.Word8 (_period, _F, _I, _T)
import Control.Monad (guard, fail)

import Data.Bits

import FIT.DataRecord.RecordHeader

-- A definition message defines the type of the upcoming data
-- messages. One defines a local message type and associates it to a
-- specific FIT message. It then designates the byte alignment and
-- field of contents of the upcoming data message.
data DefinitionMessage =
  DefinitionMessage { recordHeader :: RecordHeader
                    , definitionMessageContent :: DefinitionMessageContent
                    } deriving Show

-- instance Binary DefinitionMessage where
--   put = error "Not implemented"

--   get = do
--     header <- get :: Get.Get RecordHeader
--     content <- get :: Get DefinitionMessageContent
--     return $ DefinitionMessage header content


-- Given the RecordHeader, use it to get the definition message
getDefinitionMessage :: RecordHeader -> Get DefinitionMessage
getDefinitionMessage header = DefinitionMessage header <$> get





data DefinitionMessageContent =
  DefinitionMessageContent { architecture :: Word8
                           , globalMessageNumber :: Word16
                           , numberOfFields :: Word8
                           , fieldDefinitions :: [FieldDefinition]
                           , numberOfDeveloperFields :: Word8
                           , developerFieldDefinitions :: [DeveloperFieldDefinition]
                           } deriving Show


instance Binary DefinitionMessageContent where
  put = error "Not implemented."

  get = do
    reserved <- Get.getWord8 -- Reserved
    if reserved == 0 then return () else fail "Reserved byte is not 0"
    architecture <- Get.getWord8
    endianness <- getEndianness architecture
    globalMessageNumber <- (case endianness of
                             LittleEndian -> Get.getWord16le
                             BigEndian -> Get.getWord16be)
    numberOfFields <- Get.getWord8
    fieldDefinitions <- getFields numberOfFields
    numberOfDeveloperFields <- Get.getWord8
    developerFieldDefinitions <- getDeveloperFields numberOfDeveloperFields
    return $
      DefinitionMessageContent architecture globalMessageNumber numberOfFields fieldDefinitions numberOfDeveloperFields developerFieldDefinitions



data BaseType =
  BaseType { endianAbility :: Bool
           , baseTypeNumber :: Word8 -- The corresponding type appears
                               -- in Table 7.
           } deriving Show



data FieldDefinition =
  FieldDefinition { fieldDefinitionNumber :: Word8
                  , numberOfBytes :: Word8
                  , baseType :: BaseType
                  } deriving Show

getFields :: Integral a => a -> Get [FieldDefinition]
getFields n = getFieldsHelper n []
  where
    getFieldsHelper 0 acc = return acc
    getFieldsHelper n acc = do
      fieldDefinitionNumber <- Get.getWord8
      numberOfBytes <- Get.getWord8
      baseType <- Get.getWord8
      let
        endianAbility =
          case shiftL baseType 7 of
            0 -> False
            1 -> True

        baseTypeNumber = baseType .&. 31

      getFieldsHelper (n - 1) (FieldDefinition fieldDefinitionNumber numberOfBytes (BaseType endianAbility baseTypeNumber) : acc)


data DeveloperFieldDefinition =
  DeveloperFieldDefinition { fieldNumber :: Word8
                           , fieldSize :: Word8
                           , developerDataIndex:: Word8
                           } deriving Show

getDeveloperFields :: Integral a => a -> Get [DeveloperFieldDefinition]
getDeveloperFields n = getDeveloperFieldsHelper n []
  where
    getDeveloperFieldsHelper 0 acc = return acc
    getDeveloperFieldsHelper n acc = do
      fieldNumber <- Get.getWord8
      fieldSize <- Get.getWord8
      developerDataIndex <- Get.getWord8
      getDeveloperFieldsHelper (n - 1) (DeveloperFieldDefinition fieldNumber fieldSize developerDataIndex : acc)





getEndianness :: Word8 -> Get Endianness
getEndianness word
  | word == 0 = return LittleEndian
  | word == 1 = return BigEndian
  | otherwise = fail "Could not determine endianness"
