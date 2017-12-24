{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Text.PCF.Types (
        PCF(..),
        PCFGlyph(..),
        Prop(..),
        Table(..),
        Metrics(..),
        TableMeta(..),
        PCFTableType(..),
        PCFText(..),
        glyph_ascii,
        glyph_ascii_lines,
        glyph_braille,
        glyph_braille_lines,
        pcf_text_string,
        pcf_text_ascii,
        pcf_text_braille
    ) where

import Data.Binary
import Data.Bits
import Data.Int
import Data.Monoid
import Data.Ord (comparing)
import Data.List
import Data.Vector (Vector)
import Data.IntMap (IntMap)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString as B (unfoldrN)
import qualified Data.ByteString.Lazy as B (concatMap, take, map, index, fromStrict, length)
import qualified Data.ByteString.Lazy.Char8 as B (unpack, splitAt, intercalate, concat)
import qualified Data.Vector.Storable as VS

-- | Container of tables extracted from a PCF font file.
data PCF = PCF { pcf_properties       :: (TableMeta, Table)
               , pcf_metrics          :: (TableMeta, Table)
               , pcf_bitmaps          :: (TableMeta, Table)
               , pcf_bdf_encodings    :: (TableMeta, Table)
               , pcf_swidths          :: (TableMeta, Table)
               , pcf_accelerators     :: (TableMeta, Table)
               , pcf_glyph_names      :: Maybe (TableMeta, Table)
               , pcf_ink_metrics      :: Maybe (TableMeta, Table)
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

-- | Container of glyph dimension and position metrics.
data Metrics = Metrics  { metrics_left_sided_bearings :: Int16
                        , metrics_right_sided_bearings :: Int16
                        , metrics_character_width :: Int16
                        , metrics_character_ascent :: Int16
                        , metrics_character_descent :: Int16
                        , metrics_character_attributes :: Int16 }
    deriving (Eq, Show)

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
    deriving (Eq, Ord, Show)

-- | Container of a single glyph bitmap and its metadata.
data PCFGlyph = PCFGlyph { glyph_metrics :: Metrics
                         , glyph_char :: Char
                         -- ^ Unicode character corresponding to glyph
                         , glyph_width :: Int
                         -- ^ Pixel width of glyph once rendered
                         , glyph_height :: Int
                         -- ^ Pixel height of glyph once rendered
                         , glyph_pitch :: Int
                         -- ^ Number of bytes in each bitmap row
                         , glyph_bitmap :: ByteString
                         -- ^ `glyph_height` rows of `glyph_pitch` bytes containing the glyph's bitmap image starting from the left-most bit and ending at the `glyph_width` bit in each row
                         }

instance Show PCFGlyph where
    show g@PCFGlyph{..} = "PCFGlyph {glyph_metrics = " ++ show glyph_metrics ++
                          ", glyph_char = " ++ show glyph_char ++
                          ", glyph_width = " ++ show glyph_width ++
                          ", glyph_height = " ++ show glyph_height ++
                          ", glyph_pitch = " ++ show glyph_pitch ++
                          ", glyph_bitmap = " ++ show glyph_bitmap ++ "}\n" ++
                          glyph_ascii g

-- | Render glyph bitmap as a string where 'X' represents opaque pixels and whitespace represents blank pixels.
glyph_ascii :: PCFGlyph -> String
glyph_ascii = B.unpack . mconcat . map (<> "\n") . glyph_ascii_lines_bs

glyph_braille :: PCFGlyph -> String
glyph_braille = unlines . glyph_braille_lines

-- | Render glyph bitmap as a list of strings representing lines where 'X' represents opaque pixels and whitespace represents blank pixels.
glyph_ascii_lines :: PCFGlyph -> [String]
glyph_ascii_lines = map B.unpack . glyph_ascii_lines_bs

-- | Render glyph bitmap as a list of strings representing lines a Braille 2×4 grid of
--   pixel represents the raster.
glyph_braille_lines :: PCFGlyph -> [String]
glyph_braille_lines = map (map (toEnum . (+0x2800) . fromEnum) . B.unpack)
                       . glyph_braille_lines_bs

glyph_ascii_lines_bs :: PCFGlyph -> [ByteString]
glyph_ascii_lines_bs PCFGlyph{..} = map (B.take (fromIntegral glyph_width) . showBits) rs
    where
        rs = rows glyph_bitmap
        rows bs = case B.splitAt (fromIntegral glyph_pitch) bs of
                (r, "") -> [r]
                (r, t) -> r : rows t

        showBits = B.concatMap $ mconcat $ map showBit [7,6..0]
        showBit :: Int -> Word8 -> ByteString
        showBit i w
          | testBit w i = "X"
          | otherwise   = " "

glyph_braille_lines_bs :: PCFGlyph -> [ByteString]
glyph_braille_lines_bs PCFGlyph{..} = map showBits rs
    where
        rs = rows 4 glyph_bitmap
        rows 0 bs = [] : rows 4 bs
        rows n bs = case B.splitAt (fromIntegral glyph_pitch) bs of
                (r, "") -> [r : replicate (n-1) (const zeroBits `B.map` r)]
                (r, t) | rg:rgs <- rows (n-1) t
                          -> (r:rg) : rgs

        showBits rws = B.fromStrict . fst
                         $ B.unfoldrN (fromIntegral glyph_width`quot`2+1) build 0
         where build x = Just ( assemble $ [ index r i `testBit` fromIntegral (7-2*k-o)
                                           | o <- [0,1]
                                           , r <- take 3 rws ]
                                        ++ [ index (last rws) i
                                                         `testBit` fromIntegral (7-2*k-o)
                                           | o <- [0,1] ]
                              , x+1 )
                where (i,k) = x`divMod`4
                      index r i
                       | i < B.length r  = B.index r i
                       | otherwise       = zeroBits
                      assemble pixels = foldl' (.|.) zeroBits
                                          [ bit i | (i,px) <- zip [0..] pixels
                                                  , px ]

-- | Representation of string and its corresponding bitmap content. Metadata regarding source font is not included.
data PCFText = PCFText { pcf_text_glyphs :: [PCFGlyph]
                       -- ^ Metadata of each glyph rendered to the image bitmap
                       , pcf_text_width :: Int
                       -- ^ Width of the rendered bitmap image
                       , pcf_text_height :: Int
                       -- ^ Height of the rendered bitmap image
                       , pcf_text_image :: VS.Vector Word8
                       -- ^ Text rendered as a bitmap image where 0 is opaque and 255 is empty in each cell
                       }

-- | String represented by PCFText rendering.
pcf_text_string :: PCFText -> String
pcf_text_string = map glyph_char . pcf_text_glyphs

-- | ASCII rendering of a whole PCFText string rendering.
pcf_text_ascii :: PCFText -> String
pcf_text_ascii = unlines . map concat . transpose
            . alignBottom . map (map B.unpack) . map glyph_ascii_lines_bs . pcf_text_glyphs

-- | Braille display of a whole PCFText string rendering.
pcf_text_braille :: PCFText -> String
pcf_text_braille = unlines . map concat . transpose
                     . alignBottom . map glyph_braille_lines . pcf_text_glyphs

alignBottom :: [[String]] -> [[String]]
alignBottom l = map (\c -> replicate (hmax - length c) (map (const ' ') $ head c) ++ c) l
 where cmax = maximumBy (comparing length) l
       hmax = length cmax
