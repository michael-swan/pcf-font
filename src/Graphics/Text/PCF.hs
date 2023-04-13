{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Rendering bitmap text with __pcf-font__ is easy. For instance, a program that renders text into a PNG is trivial:
--
-- > import Codec.Picture.Png
-- > import Codec.Picture.Types
-- > import Data.List
-- > import Graphics.Text.PCF
-- > import System.Environment
-- >
-- > -- | USAGE: program <font.pcf.gz> <output.png> <text>
-- > main :: IO ()
-- > main = do
-- >     [input_file, output_file, text] <- getArgs
-- >     pcf <- either fail return =<< loadPCF input_file
-- >     case renderPCFText pcf text of
-- >         Just (PCFText _ w h img) ->
-- >             writePng output_file (Image w h img :: Image Pixel8)
-- >         Nothing ->
-- >             putStrLn "ERROR: Unable to render input text."
--
-- Rendering some text as an ASCII bitmap is also convenient:
--
-- > import Graphics.Text.PCF
-- > import System.Environment
-- >
-- > -- | USAGE: program <font.pcf.gz> <text>
-- > main :: IO ()
-- > main = do
-- >     [font_file, text] <- getArgs
-- >     pcf <- either fail return =<< loadPCF font_file
-- >     case renderPCFText pcf text of
-- >         Just pcf_text ->
-- >             putStrLn $ pcf_text_ascii pcf_text
-- >         Nothing ->
-- >             putStrLn "ERROR: Unable to render input text."
module Graphics.Text.PCF (
        -- * Decoding
        loadPCF,
        decodePCF,
        -- * Rendering
        renderPCFText,
        renderPCFTextColor,
        getPCFGlyph,
        getPCFGlyphPixel,
        foldPCFGlyphPixels,
        -- * ASCII Rendering
        pcf_text_ascii,
        glyph_ascii,
        glyph_ascii_lines,
        -- * Metadata
        getPCFProps,
        -- * Types
        PCF,
        PCFGlyph(..),
        PCFText(..),
        Metrics(..)
    ) where

import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Bool
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import GHC.Exts
import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.IntMap as IntMap
import Graphics.Text.PCF.Types
import Codec.Compression.GZip

assert :: MonadFail m => Bool -> String -> m ()
assert True  = const $ return ()
assert False = fail

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

-- | List key-value pairs found in PCF properties table.
getPCFProps :: PCF -> [(ByteString, Either ByteString Int)]
getPCFProps PCF{..} = flip map properties_props $ \Prop{..} ->
        (getPropString prop_name_offset,
         if prop_is_string /= 0 then
             Left $ getPropString prop_value
         else
             Right $ fromIntegral prop_value)
    where
        (_, PROPERTIES{..}) = pcf_properties
        getPropString = B.takeWhile (/= 0) . flip B.drop properties_strings . fromIntegral

-- | Extract a single glyph bitmap from a PCF font.
getPCFGlyph :: PCF -> Char -> Maybe PCFGlyph
getPCFGlyph PCF{..} c = do
        glyph_index <- fromIntegral <$> IntMap.lookup (ord c) encodings_glyph_indices
        offset      <- fromIntegral <$> (bitmaps_offsets V.!? glyph_index)
        m@Metrics{..} <- metrics_metrics V.!? glyph_index
        let cols = fromIntegral $ metrics_right_sided_bearings - metrics_left_sided_bearings
            rows = fromIntegral $ metrics_character_ascent + metrics_character_descent
        pitch <- case tableMetaGlyphPad meta_bitmaps of
                    1 -> Just $ (cols + 7) `shiftR` 3
                    2 -> Just $ ((cols + 15) `shiftR` 4) `shiftL` 1
                    4 -> Just $ ((cols + 31) `shiftR` 5) `shiftL` 2
                    8 -> Just $ ((cols + 63) `shiftR` 6) `shiftL` 3
                    _ -> Nothing
        let bytes = fromIntegral $ rows * pitch
        return $ PCFGlyph m c cols rows pitch (B.take bytes $ B.drop offset bitmaps_data)
    where
        (meta_bitmaps, BITMAPS{..}) = pcf_bitmaps
        (_, METRICS{..})            = pcf_metrics
        (_, BDF_ENCODINGS{..})      = pcf_bdf_encodings

getPCF :: Get PCF
getPCF = do
    magic <- getByteString 4
    assert (magic == "\1fcp")
        "Invalid magic number found in PCF header."
    -- Table count silently capped at 9 for compatibility with FreeType
    table_count <- min 9 <$> getWord32le
    table_metas <- sortWith tableMetaOffset <$> replicateM (fromIntegral table_count) getTableMeta
    let table_in_bounds (t0, t1) = tableMetaSize t0 <= tableMetaOffset t1 &&
                                   tableMetaOffset t0 <= tableMetaOffset t1 - tableMetaSize t0
        table_types = map tableMetaType table_metas
    assert (all table_in_bounds $ zip table_metas $ tail table_metas)
        "Multiple PCF tables overlap, according to metadata."
    assert (allUnique table_types)
        "Multiple PCF tables of the same type is not supported."
    tables <- mapM get_table table_metas
    let tableMap = flip M.lookup $ M.fromList $ zip table_types $ zip table_metas tables
        pcf = PCF <$> tableMap PCF_PROPERTIES
                  <*> tableMap PCF_METRICS
                  <*> tableMap PCF_BITMAPS
                  <*> tableMap PCF_BDF_ENCODINGS
                  <*> tableMap PCF_SWIDTHS
                  <*> (tableMap PCF_BDF_ACCELERATORS <|> tableMap PCF_ACCELERATORS)
                  <*> pure (tableMap PCF_GLYPH_NAMES)
                  <*> pure (tableMap PCF_INK_METRICS)
        missing = filter (isNothing . tableMap)
                    [ PCF_PROPERTIES
                    , PCF_ACCELERATORS
                    , PCF_METRICS
                    , PCF_BITMAPS
                    , PCF_INK_METRICS
                    , PCF_BDF_ENCODINGS
                    , PCF_SWIDTHS
                    , PCF_GLYPH_NAMES
                    , PCF_BDF_ACCELERATORS ]
    maybe (fail $ "Incomplete PCF given. One or more tables are missing: " ++ show missing) return pcf
    where
      isDefaultFormat, isAccelWithInkBoundsFormat, isCompressedMetricsFormat :: Word32 -> Bool
      isDefaultFormat = (== 0x00000000) . (.&. 0xFFFFFF00)
      isAccelWithInkBoundsFormat = (== 0x00000100) . (.&. 0xFFFFFF00)
      isCompressedMetricsFormat = (== 0x00000100) . (.&. 0xFFFFFF00)

      get_table TableMeta{..} = do
        pos <- bytesRead
        skip $ fromIntegral tableMetaOffset - fromIntegral pos
        _ <- getWord32le -- Redundant 'format' field.
        let getWord32 = if tableMetaByte then getWord32be else getWord32le
        let getWord16 = if tableMetaByte then getWord16be else getWord16le
        let getInt16 = if tableMetaByte then getInt16be else getInt16le
        let get_metrics = Metrics <$> getInt16 <*> getInt16 <*> getInt16 <*> getInt16 <*> getInt16 <*> getInt16
        let get_metrics_table ty = do
                assert (isDefaultFormat tableMetaFormat || isCompressedMetricsFormat tableMetaFormat)
                    "Properties table only supports PCF_DEAULT_FORMAT and PCF_COMPRESSED_METRICS."
                metrics <- fmap V.fromList $ if isCompressedMetricsFormat tableMetaFormat then do
                  metrics_count <- getWord16
                  let getInt = fmap (\x -> fromIntegral $ x - 127 - 1) getInt8
                  replicateM (fromIntegral metrics_count) $
                    Metrics <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> pure 0
                else do
                  metrics_count <- getWord32
                  replicateM (fromIntegral metrics_count) get_metrics

                return $ METRICS ty (isCompressedMetricsFormat tableMetaFormat) metrics
        let get_accelerators_table = 
              ACCELERATORS <$> get <*> get <*> get <*> get <*> get <*> get <*> get
                           <* getWord8 <*> getWord32 <*> getWord32 <*> getWord32 <*> get_metrics <*> get_metrics
                           <*> (if isAccelWithInkBoundsFormat tableMetaFormat then
                                  fmap Just $ (,) <$> get_metrics <*> get_metrics
                                else
                                  pure Nothing)
        table <- case tableMetaType of
          PCF_PROPERTIES -> do
            assert (isDefaultFormat tableMetaFormat)
              "Properties table only supports PCF_DEFAULT_FORMAT."
            nprops <- getWord32
            props <- replicateM (fromIntegral nprops) (Prop <$> getWord32 <*> getWord8 <*> getWord32)
            skip $ (4 - fromIntegral nprops `mod` 4) `mod` 4 -- Insert padding
            string_size <- getWord32
            strings <- getByteString (fromIntegral string_size)
            return $ PROPERTIES props (B.fromStrict strings)
          PCF_ACCELERATORS     -> get_accelerators_table
          PCF_BDF_ACCELERATORS -> get_accelerators_table
          PCF_METRICS     -> get_metrics_table False
          PCF_INK_METRICS -> get_metrics_table True
          PCF_BITMAPS -> do
            glyph_count <- getWord32
            offsets <- V.fromList <$> replicateM (fromIntegral glyph_count) getWord32
            sizes <- (,,,) <$> getWord32 <*> getWord32 <*> getWord32 <*> getWord32
            bitmap_data <- case (tableMetaGlyphPad, sizes) of
                             (1, (w,_,_,_)) -> getByteString $ fromIntegral $ w
                             (2, (_,x,_,_)) -> getByteString $ fromIntegral $ x
                             (4, (_,_,y,_)) -> getByteString $ fromIntegral $ y
                             (8, (_,_,_,z)) -> getByteString $ fromIntegral $ z
                             _ -> fail "Invalid glyph padding encountered while parsing PCF bitmap table."
            return $ BITMAPS glyph_count offsets sizes (B.fromStrict bitmap_data)
          PCF_BDF_ENCODINGS -> do
            cols <- (,) <$> getWord16 <*> getWord16
            rows <- (,) <$> getWord16 <*> getWord16
            default_char <- getWord16
            glyph_indices <-
                flip mapM [fst rows..snd rows] $ \i ->
                    flip mapM [fst cols..snd cols] $ \j -> do
                        encoding_offset <- getWord16
                        return (fromIntegral $ i * 256 + j, encoding_offset)
            return $ BDF_ENCODINGS cols rows default_char (IntMap.fromList $ concat glyph_indices)
          PCF_SWIDTHS -> do
            glyph_count <- getWord32
            SWIDTHS <$> replicateM (fromIntegral glyph_count) getWord32
          PCF_GLYPH_NAMES ->
            GLYPH_NAMES <$> (getWord32 >>= flip replicateM getWord32 . fromIntegral) <*> (getWord32 >>= fmap B.fromStrict . getByteString . fromIntegral)
        return table

-- | Load a PCF font file. Both uncompressed and GZip compressed files are allowed, i.e. ".pcf" and ".pcf.gz" files.
loadPCF :: FilePath -> IO (Either String PCF)
loadPCF filepath = decodePCF <$> B.readFile filepath

-- | Decode a PCF font from an in-memory `ByteString`. Uncompressed and GZip compressed input are allowed.
decodePCF :: ByteString -> Either String PCF
decodePCF bs = either (Left . extract) (Right . extract) $ runGetOrFail getPCF $ if B.take 2 bs == "\x1f\x8b" then decompress bs else bs
    where
        extract (_,_,v) = v

getPCFTableType :: Get PCFTableType
getPCFTableType = do
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
    _     -> fail "Invalid PCF table type encountered."

getTableMeta :: Get TableMeta
getTableMeta = do
  table_type <- getPCFTableType
  fmt <- getWord32le
  size <- getWord32le
  offset <- getWord32le
  return $ TableMeta table_type fmt (shiftL 1 $ fromIntegral $ fmt .&. 3) (fromIntegral $ fmt `shiftR` 4 .&. 0x3) (testBit fmt 2) (testBit fmt 3) size offset

-- | Calculate the color of a pixel in a glyph given its (x,y) coordinates.
getPCFGlyphPixel :: PCFGlyph
                 -> Int
                 -- ^ X
                 -> Int
                 -- ^ Y
                 -> Bool
                 -- ^ `True` if pixel at (x,y) is opaque; `False` if pixel at (x,y) is transparent or (x,y) is out of the glyph's bounds
getPCFGlyphPixel g@PCFGlyph{..} x y = x < glyph_width && y < glyph_height && x >= 0 && y >= 0 && getPCFGlyphPixelUnsafe g x y

getPCFGlyphPixelUnsafe :: PCFGlyph -> Int -> Int -> Bool
getPCFGlyphPixelUnsafe PCFGlyph{..} x y = testBit (B.head $ B.drop off glyph_bitmap) (7 - x `mod` 8)
    where
        off = fromIntegral $ y * glyph_pitch + x `div` 8
        

-- | Scan over every pixel in a glyph, constructing some value in the process.
foldPCFGlyphPixels :: PCFGlyph
                   -> (Int -> Int -> Bool -> a -> a)
                   -- ^ Function that takes x, y, pixel value at (x,y), and an accumulator, returning a modified accumulator
                   -> a
                   -- ^ Initial accumulator
                   -> a
foldPCFGlyphPixels g@PCFGlyph{..} f =
    fold [0..glyph_width-1] $ \x ->
        fold [0..glyph_height-1] $ \y ->
            f x y (getPCFGlyphPixelUnsafe g x y)
    where
        fold bs f' a = foldl' (flip f') a bs

-- | Generate a vector of black and white pixels from a PCF font and a string. Black and white pixels are represented by 0x00 and 0xFF byte values respectively.
renderPCFText :: PCF
              -- ^ Font to render with
              -> String
              -- ^ Text to render
              -> Maybe PCFText
              -- ^ `Just` width, height, and rendering; `Nothing` if an unrenderable character is encountered
renderPCFText pcf = renderPCFTextColor pcf 0x00 0xFF

-- | Generate a vector of opaque and blank pixels from a PCF font and a string.
renderPCFTextColor :: PCF
                   -- ^ Font to render with
                   -> Word8
                   -- ^ Opaque color value
                   -> Word8
                   -- ^ Blank color value
                   -> String
                   -- ^ Text to render
                   -> Maybe PCFText
                   -- ^ `Just` width, height, and rendering; `Nothing` if an unrenderable character is encountered
renderPCFTextColor pcf@PCF{..} opaque blank text = do
    glyphs <- mapM (getPCFGlyph pcf) text
    let w = foldl' (\n -> (n +) . fromIntegral . metrics_character_width . glyph_metrics) 0 glyphs
        ascent = foldl' (\n PCFGlyph{..} -> max n (metrics_character_ascent glyph_metrics)) 0 glyphs
        descent = foldl' (\n PCFGlyph{..} -> max n (metrics_character_descent glyph_metrics)) 0 glyphs
        h = fromIntegral $ ascent + descent
        updates _ [] = []
        updates off (g:gs) = foldPCFGlyphPixels g (\x y -> bool id (off + x + fromIntegral (metrics_left_sided_bearings (glyph_metrics g)) + (y + fromIntegral (ascent - metrics_character_ascent (glyph_metrics g))) * w:)) [] : updates (off + fromIntegral (metrics_character_width (glyph_metrics g))) gs
    -- 64 MB max image size
    if w * h > 64 * 1024 * 1024 then
        Nothing
    else
        return (PCFText glyphs w h $ VS.replicate (w * h) blank VS.// (map (,opaque) $ concat $ updates 0 glyphs))
