{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.Text.PCF.Types (PCF(..), PCFGlyph(..), Prop(..), Table(..), Metrics(..), TableMeta(..), PCFTableType(..)) where

import Data.Binary
import Data.Bits
import Data.Monoid
import Data.Vector (Vector)
import Data.IntMap (IntMap)
import Data.ByteString.Lazy as B (concatMap)
import Data.ByteString.Lazy.Char8 as B (ByteString, unpack, splitAt)

data PCF = PCF { pcf_properties       :: (TableMeta, Table)
               , pcf_accelerators     :: (TableMeta, Table)
               , pcf_metrics          :: (TableMeta, Table)
               , pcf_bitmaps          :: (TableMeta, Table)
               , pcf_ink_metrics      :: (TableMeta, Table)
               , pcf_bdf_encodings    :: (TableMeta, Table)
               , pcf_swidths          :: (TableMeta, Table)
               , pcf_glyph_names      :: (TableMeta, Table)
               , pcf_bdf_accelerators :: (TableMeta, Table)
               }

data Table = PROPERTIES { properties_props :: [Prop]
                        , properties_strings :: ByteString }
           | BITMAPS { bitmaps_glyph_count :: Word32
                     , bitmaps_offsets :: Vector Word32
                     , bitmaps_sizes :: (Word32, Word32, Word32, Word32)
                     , bitmaps_data :: ByteString }
           | METRICS { metrics_ink_type :: Bool
                     , metrics_compressed :: Bool
                     , metrics_metrics :: Vector Metrics }
           | SWIDTHS { swidths_swidths :: [Word32] }
           | ACCELERATORS { accel_no_overlap :: Bool
                          , accel_constant_metrics :: Bool
                          , accel_terminal_font :: Bool
                          , accel_constant_width :: Bool
                          , accel_ink_inside :: Bool
                          , accel_ink_metrics :: Bool
                          , accel_draw_direction :: Bool
                          -- ^ False = left to right, True = right to left
                          , accel_font_ascent :: Word32
                          , accel_font_descent :: Word32
                          , accel_max_overlap :: Word32
                          , accel_min_bounds :: Metrics
                          , accel_max_bounds :: Metrics
                          , accel_ink_min_max_bounds :: Maybe (Metrics, Metrics)
                          }
           | GLYPH_NAMES { glyph_names_offsets :: [Word32]
                         , glyph_names_string :: ByteString }
           | BDF_ENCODINGS { encodings_cols :: (Word16, Word16)
                           , encodings_rows :: (Word16, Word16)
                           , encodings_default_char :: Word16
                           , encodings_glyph_indices :: IntMap Word16 }

data Prop = Prop { prop_name_offset :: Word32
                 , prop_is_string :: Word8
                 , prop_value :: Word32 }
    deriving (Eq)

data Metrics = Metrics  { metrics_left_sided_bearings :: Word16
                        , metrics_right_sided_bearings :: Word16
                        , metrics_character_width :: Word16
                        , metrics_character_ascent :: Word16
                        , metrics_character_descent :: Word16
                        , metrics_character_attributes :: Word16 }
    deriving (Eq)

data TableMeta = TableMeta { tableMetaType :: PCFTableType
                           -- ^ Table type
                           , tableMetaFormat :: Word32
                           -- ^ Whole format field for reconstructing
                           , tableMetaGlyphPad :: Word8
                           -- ^ Level of padding applied to glyph bitmaps
                           , tableMetaScanUnit :: Word8
                           -- ^ ?
                           , tableMetaByte :: Bool
                           -- ^ Byte-wise endianess
                           , tableMetaBit :: Bool
                           -- ^ Bit-wise endianess
                           , tableMetaSize :: Word32
                           -- ^ Number of bytes used by the table
                           , tableMetaOffset :: Word32
                           -- ^ Byte offset to table from beginning of file
                           }

data PCFTableType = PCF_PROPERTIES
                  | PCF_ACCELERATORS
                  | PCF_METRICS
                  | PCF_BITMAPS
                  | PCF_INK_METRICS
                  | PCF_BDF_ENCODINGS
                  | PCF_SWIDTHS
                  | PCF_GLYPH_NAMES
                  | PCF_BDF_ACCELERATORS
    deriving (Eq, Ord)

data PCFGlyph = PCFGlyph { glyph_char :: Char
                         , glyph_width :: Int
                         , glyph_height :: Int
                         , glyph_padding :: Int
                         , glyph_bitmap :: ByteString }

instance Show PCFGlyph where
    show PCFGlyph{..} = "PCFGlyph {glyph_char = " ++ show glyph_char ++
                        ", glyph_width = " ++ show glyph_width ++
                        ", glyph_height = " ++ show glyph_height ++
                        ", glyph_bitmap = " ++ show glyph_bitmap ++ "}\n" ++

                        (unpack $ mconcat $ map ((<> "\n") . showBits) rs)
        where
            
            rs = rows glyph_bitmap
            rows bs = case B.splitAt pitch bs of
                    (r, "") -> [r]
                    (r, t) -> r : rows t
                    
            pitch = fromIntegral $ case glyph_padding of
                        1 -> (glyph_width + 7) `shiftR` 3
                        2 -> (glyph_width + 15) `shiftR` 4 `shiftL` 1
                        4 -> (glyph_width + 31) `shiftR` 5 `shiftL` 2
                        8 -> (glyph_width + 63) `shiftR` 6 `shiftL` 3
                        _ -> 4

            showBits = B.concatMap (\w -> showBit w 7 <> showBit w 6 <> showBit w 5 <> showBit w 4 <> showBit w 3 <> showBit w 2 <> showBit w 1 <> showBit w 0)
            showBit :: Word8 -> Int -> ByteString
            showBit w i
              | testBit w i = "X"
              | otherwise   = " "

