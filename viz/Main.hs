module Main where

import Control.Applicative
import Data.Word
import Data.Bits
import Data.Array
import Codec.Picture
import qualified Data.Vector as V
import System.Random.MWC

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Hashable as Their
import qualified Data.Hashabler as Our

-- A quick tool to visualize distributions. 
--
-- All pretty inefficient; we could build the Image vector directly, and work
-- with random mutable vectors as input.


main :: IO ()
main = do
    let out nm = "viz/out/"++nm++".bmp"
        viz nm s = do saveBmpImage (out nm) $ bitmap s
                      putStrLn $ "Wrote "++(out nm)

    -- Visualize true uniform randomness 
    vs <- withSystemRandom . asGenST $ \gen -> uniformVector gen (1024*1024)
    let samples = V.toList vs
    viz "random" samples

    -- should look like a red square 1/256 of side length:
    --viz "span16" $ map fromIntegral [(0::Word16).. maxBound]
    -- Should look flat light gray:
    --viz "span_all_px" $ [0,(64*64).. maxBound]

{-  NOT SO INTERESTING --------

    -- Int
    viz "hashable_Word32" $ map ((fromIntegral :: Int -> Word32) . Their.hash) samples
    viz "hashabler_Word32" $ map (Our.hashWord32 . Our.hashFNV32) samples

    -- Word8
    samples <- V.toList <$> (withSystemRandom . asGenST $ \gen -> uniformVector gen (1024*1024))
    viz "hashable_Word8" $ map (fromIntegral . Their.hash) (samples :: [Word8])
    viz "hashabler_Word8" $ map (Our.hashWord32 . Our.hashFNV32) samples
 
 -}    

    -- (Word8 , Word8)
    samples <- V.toList <$> (withSystemRandom . asGenST $ \gen -> uniformVector gen (1024*1024))
    viz "Word8,Word8_hashable" $ map (fromIntegral . Their.hash) (samples :: [(Word8,Word8)])
    viz "Word8,Word8_hashabler" $ map (Our.hashWord32 . Our.hashFNV32) samples
    
    -- (Word8, Word8 , Word8)
    samples <- V.toList <$> (withSystemRandom . asGenST $ \gen -> uniformVector gen (1024*1024))
    viz "Word8,Word8,Word8_hashable" $ map (fromIntegral . Their.hash) (samples :: [(Word8,Word8,Word8)])
    viz "Word8,Word8,Word8_hashabler" $ map (Our.hashWord32 . Our.hashFNV32) samples

    -- [Ordering]  -- exhaustive, of length 10 (~59K)
    let samples 0 = [[]]
        samples n = let ss = samples (n-1)
                     in map (LT:) ss ++ map (GT:) ss ++ map (EQ:) ss
    viz "List_10_Ordering_hashable" $ map (fromIntegral . Their.hash) $ samples 10
    viz "List_10_Ordering_hashabler" $ map (Our.hashWord32 . Our.hashFNV32) $ samples 10

    -- [Either Int Bool] -- of random length 0 .. 100

    -- String  -- 100K english words
    samples <- lines <$> readFile "/usr/share/dict/words"
    viz "String_words_hashable" $ map (fromIntegral . Their.hash) samples
    viz "String_words_hashabler" $ map (Our.hashWord32 . Our.hashFNV32) samples

    -- Text    -- 100K english words
    samples <- T.lines <$> T.readFile "/usr/share/dict/words"
    viz "Text_words_hashable" $ map (fromIntegral . Their.hash) samples
    viz "Text_words_hashabler" $ map (Our.hashWord32 . Our.hashFNV32) samples

    {-
    -- Text    -- 4K english words, mostly starting with A
    samples <- take 4000 . T.lines <$> T.readFile "/usr/share/dict/words"
    viz "Text_similar_words_hashable" $ map (fromIntegral . Their.hash) samples
    viz "Text_similar_words_hashabler" $ map (Our.hashWord32 . Our.hashFNV32) samples
    -}




bitmap :: [ Word32 ] -> DynamicImage
bitmap samples = ImageRGB8 $ generateImage (\x y-> a!(x,y)) 1024 1024
    where a = sampleArr samples

-- Our bitmap in array form ( would be nice if we could do this in juicypixels
-- directly)
sampleArr :: [ Word32 ] -> Array (Int,Int) PixelRGB8
sampleArr = accumArray darkenOrClipping white ((0,0) , (1023,1023)) . mapH where
    darkenOrClipping (PixelRGB8 r g b) _
        -- Red to indicate clipping; this is stable
        | (minBound+darkenIncr) > g = PixelRGB8 maxBound 0 0 
        | otherwise     = PixelRGB8 (r-darkenIncr) (g-darkenIncr) (b-darkenIncr)
    white = PixelRGB8 maxBound maxBound maxBound
    darkenIncr = 64
    mapH = map (\d-> (coord d, ()))


-- Map a Word32 to a point on 1024*1024 hilbert curve filled plane.
coord :: Word32 -> (Int,Int)
coord n = 
    -- First we quantize n to a 64x64 square. Later we'll use pixel color to
    -- indicate number of members.
    let n64 = fromIntegral $ n `div` (64*64)
     in hilbert 1024 n64


-- Copied from http://en.wikipedia.org/wiki/Hilbert_curve#Applications_and_mapping_algorithms
hilbert :: Int -> Int -> (Int,Int)
hilbert n = go 0 0 1 where
    go x y s t
        | s >= n = (x,y)
        | otherwise = 
            let rx = 1 .&. (t `div` 2)
                ry = 1 .&. (t `xor` rx)
                (x', y') = rot s x y rx ry
             
             in go (x' + s*rx) (y' + s*ry) (s*2) (t `div` 4)

    rot :: Int -> Int -> Int -> Int -> Int -> (Int,Int)
    rot s x y rx ry
        | ry == 0 = 
              if rx == 1  
                 then (s-1 - y, s-1 - x )
                 else (y, x) 
        | otherwise = (x, y)
