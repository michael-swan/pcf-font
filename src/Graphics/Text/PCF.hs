{-# LANGUAGE OverloadedStrings #-}
module Graphics.Text.PCF (PCF, loadPCF, decodePCF) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Bool
import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

data PCF = PCF { pcfTables :: [PCFTable] }
    deriving (Show)

instance Binary PCF where
  get = do
    magic <- getByteString 4
    case magic == "\1fcp" of
      True  -> return ()
      False -> error "Invalid magic number found in PCF header."
    table_count <- fromIntegral <$> getWord32le
    PCF <$> (sequence $ take table_count $ repeat get)

  put (PCF tables) = do
    putByteString "\1fcp"
    putWord32le $ fromIntegral $ length tables
    mapM_ put tables
    
    

data PCFTable = PCFTable { pcfTableType :: PCFTableType
                         , pcfTableFormat :: PCFTableFormat
                         , pcfTableFormatMasks :: [PCFTableFormatMask]
                         , pcfTableSize :: Word32
                         , pcfTableOffset :: Word32
                         }
    deriving (Show)

instance Binary PCFTable where
  get = do
    table_type <- get
    fmt <- getWord32le
    size <- getWord32le
    offset <- getWord32le
    return $ PCFTable
      table_type
      (if testBit fmt 9 then
         case table_type of
           PCF_ACCELERATORS -> PCF_ACCEL_W_INKBOUNDS
           PCF_METRICS      -> PCF_COMPRESSED_METRICS
       else if testBit fmt 8 then
         PCF_INKBOUNDS
       else
         PCF_DEFAULT_FORMAT)
      (bool [PCF_GLYPH_PAD_MASK] [] (testBit fmt 0 && testBit fmt 1)
       ++ bool [PCF_BYTE_MASK] [] (testBit fmt 2)
       ++ bool [PCF_BIT_MASK] [] (testBit fmt 3)
       ++ bool [PCF_SCAN_UNIT_MASK] [] (testBit fmt 4 && testBit fmt 5))
      size
      offset

  put (PCFTable ty fmt masks size offset) = do
    put ty
    let mask = foldl' (\n mask ->
                 n .&. case mask of
                   PCF_GLYPH_PAD_MASK -> 0x03
                   PCF_BYTE_MASK      -> 0x04
                   PCF_BIT_MASK       -> 0x08
                   PCF_SCAN_UNIT_MASK -> 0x30) 0 masks
    putWord32le $ mask .|.
      case fmt of
        PCF_DEFAULT_FORMAT     -> 0x000
        PCF_INKBOUNDS          -> 0x100
        PCF_ACCEL_W_INKBOUNDS  -> 0x200
        PCF_COMPRESSED_METRICS -> 0x200
    putWord32le size
    putWord32le offset


data PCFTableType = PCF_PROPERTIES
                  | PCF_ACCELERATORS
                  | PCF_METRICS
                  | PCF_BITMAPS
                  | PCF_INK_METRICS
                  | PCF_BDF_ENCODINGS
                  | PCF_SWIDTHS
                  | PCF_GLYPH_NAMES
                  | PCF_BDF_ACCELERATORS
    deriving (Show)

instance Binary PCFTableType where
  get = do
    type_rep <- getWord32le
    case type_rep of
      0x001 -> return PCF_PROPERTIES
      0x002 -> return PCF_ACCELERATORS
      0x004 -> return PCF_METRICS
      0x008 -> return PCF_BITMAPS
      0x010 -> return PCF_INK_METRICS
      0x020 -> return PCF_BDF_ENCODINGS
      0x040 -> return PCF_SWIDTHS
      0x080 -> return PCF_GLYPH_NAMES
      0x100 -> return PCF_BDF_ACCELERATORS
      _     -> fail "Invalid PCF table type."

  put type_val = putWord32le $
      case type_val of
        PCF_PROPERTIES       -> 0x001 
        PCF_ACCELERATORS     -> 0x002 
        PCF_METRICS          -> 0x004 
        PCF_BITMAPS          -> 0x008 
        PCF_INK_METRICS      -> 0x010 
        PCF_BDF_ENCODINGS    -> 0x020 
        PCF_SWIDTHS          -> 0x040 
        PCF_GLYPH_NAMES      -> 0x080 
        PCF_BDF_ACCELERATORS -> 0x100 

data PCFTableFormat = PCF_DEFAULT_FORMAT
                    | PCF_INKBOUNDS
                    | PCF_ACCEL_W_INKBOUNDS
                    | PCF_COMPRESSED_METRICS
    deriving (Show)

data PCFTableFormatMask = PCF_GLYPH_PAD_MASK
                        | PCF_BYTE_MASK
                        | PCF_BIT_MASK
                        | PCF_SCAN_UNIT_MASK
    deriving (Show)



loadPCF :: FilePath -> IO (Either String PCF)
loadPCF filepath = decodePCF <$> B.readFile filepath

decodePCF :: ByteString -> Either String PCF
decodePCF bs =
  case decodeOrFail bs of
    Left (_, _, err) -> Left err
    Right (_, _, pcf) -> Right pcf
