{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main ( 
    main
   ) where

import Criterion.Main
import Data.Word
import Data.Int
import Data.Ratio
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Primitive as P
import Control.DeepSeq

import Data.Hashabler
import qualified Data.Hashable as Their


instance NFData FNV32 where rnf = rnf . fnv32 
instance NFData (Hash32 a) where rnf = rnf . hashWord32
instance NFData (Hash64 a) where rnf = rnf . hashWord64

-- BASELINE for list instances: 
-- a fused foldl' equivalent -- NOTE ~ 2x faster than Unfolded on 7.10
hashLeftNoList :: FNV32 -> Word8 -> Word32  -- NOTE: tested w/ this monomorphic sig
{-# INLINE hashLeftNoList #-}
hashLeftNoList = go
    where go !h 0 = fnv32 h
          go !h !b = go (h `hash` b) (b-1)

-- raise hash benchmarks above criterion noise floor from function call and nf.
hash32Times :: Hashable a=> Int -> a -> Word32
{-# INLINE hash32Times #-}
hash32Times iters =
  \a->  let go !h !0 = h
            go !h !n = go (h `hash` a) (n-1)
         in fnv32 $ go fnvOffsetBasis32 iters 

main :: IO ()
main = do
#  ifdef ASSERTIONS_ON
    error "Sorry, please reconfigure without -finstrumented so that we turn off assertions in library code."
#  endif

    let bs50 = B.pack $ replicate 48 1
        bs1000 = B.pack $ replicate 1000 1
        t50 = T.pack $ replicate 25 'a' -- TODO verify this is 50 bytes
        t1000 = T.pack $ replicate 500 'a' -- TODO verify this is 1000 bytes
        --
        bs1 = B.pack $ replicate 1 1
        bs2 = B.pack $ replicate 2 1
        bs3 = B.pack $ replicate 3 1
        bs4 = B.pack $ replicate 4 1
        bs5 = B.pack $ replicate 5 1
        bs6 = B.pack $ replicate 6 1
        bs7 = B.pack $ replicate 7 1
        bs8 = B.pack $ replicate 8 1
        bs9 = B.pack $ replicate 9 1
        bs11 = B.pack $ replicate 11 1
        bs16 = B.pack $ replicate 16 1
        bs32 = B.pack $ replicate 32 1
        bs64 = B.pack $ replicate 64 1
        bs128 = B.pack $ replicate 128 1
        bs256 = B.pack $ replicate 256 1
        bs512 = B.pack $ replicate 512 1
        bs1024 = B.pack $ replicate 1024 1

    ba50 <- P.newByteArray 50 >>= \ba'-> P.fillByteArray ba' 0 50 1 >> P.unsafeFreezeByteArray ba'
    ba50Aligned <- P.newAlignedPinnedByteArray 50 (P.alignment (undefined::Word64)) >>= \ba'-> P.fillByteArray ba' 0 50 1 >> P.unsafeFreezeByteArray ba'
    ba50AlignedBadly <- P.newAlignedPinnedByteArray 50 (7) >>= \ba'-> P.fillByteArray ba' 0 50 1 >> P.unsafeFreezeByteArray ba'
    ba1000 <- P.newByteArray 1000 >>= \ba'-> P.fillByteArray ba' 0 1000 1 >> P.unsafeFreezeByteArray ba'
    -- lazy Text and ByteString:
    let bs50LazyTrivial = BL.fromStrict bs50
        bs1000Lazy_by20Chunks = BL.fromChunks $ replicate 20 bs50
    let t50LazyTrivial = TL.fromStrict t50
        t1000Lazy_by20Chunks = TL.fromChunks $ replicate 20 t50


    let {-# INLINE byt #-}
        byt = 0xFF :: Word8

    -- TODO new set of list benchmarks:
    --        - different ways of constructing as "good producers" (and not)
    --        - different element types (e.g. Enum of Int vs Word32)
    --        - different syntactic variations, w/ eta expansion etc.
    let listBgroup nm sz = bgroup nm [
            -- ideal code, for baseline:
              bench "hashLeftNoList" $ nf (hashLeftNoList fnvOffsetBasis32) sz
            -- ...and this only  a few % faster:
          --, bench "hashLeftUnfoldedNoList" $ nf (hashLeftUnfoldedNoList fnvOffsetBasis32) sz
            -- Slow:
         -- , bench "hashFoldr" $ nf (hashFoldr fnvOffsetBasis32) [1..250]
         -- , bench "hashFoldr trying to fuse" $ nf (\i-> hashFoldr fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) 1
            -- Slow:
            , bench "hashFoldl'" $ nf (hashFoldl' fnvOffsetBasis32) [1..sz]
            -- !!! As fast as hashLeftNoList -- TODO is there really a rule firing here?
            , bench "hashFoldl' trying to fuse" $ nf (\i-> hashFoldl' fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) (1::Word8)
            -- adding extra multiply adds ~ 65% overhead!:
          --, bench "hashFoldl'Extra trying to fuse" $ nf (\i-> hashFoldl'Extra fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) 1
            -- Faster than un-fused (I think that's what's happening) fold-based code:
            , bench "hashLeftUnfolded" $ nf (hashLeftUnfolded fnvOffsetBasis32) [1..sz]
          --, bench "hashLeftUnfolded trying to fuse" $ nf (\i-> hashLeftUnfolded fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) 1
            ]

    allWordsText <- T.readFile "/usr/share/dict/words"
    let allWordsListText = T.lines allWordsText

    defaultMain [ 
     bgroup "compare" [
        bench "long Text, hashable" $ nf Their.hash allWordsText
      , bench "[Text], hashable" $  nf Their.hash allWordsListText
      , bench "long Text, hashabler" $ nf (hashWord32 . hashFNV32) allWordsText
      , bench "[Text], hashabler" $  nf (hashWord32 . hashFNV32) allWordsListText
      -- TODO ByteString
      ],
     bgroup "compare with hashable" [
        hashableBenchmarkTheir "hashable"
      , hashableBenchmarkFNV64
      , hashableBenchmarkSiphash64
     ],
     bgroup "dev" [
        -- We can more or less subtract this from benchmarks producing a Word32 hash:

        bench "baseline Word32" $ nf (\x-> x) (777::Word32)
      , bgroup "hashFNV32 on small primitive values x100" [
          -- platform-dependent:
            bench "Int" $ nf (hash32Times 100) (9999::Int)
          , bench "Word" $ nf (hash32Times 100) (9999::Word)
          , bench "Int8" $ nf (hash32Times 100) (maxBound :: Int8)
          , bench "Word8" $ nf (hash32Times 100) (maxBound :: Word8)
          , bench "Int16" $ nf (hash32Times 100) (maxBound :: Int16)
          , bench "Word16" $ nf (hash32Times 100) (maxBound :: Word16)
          , bench "Int32" $ nf (hash32Times 100) (maxBound :: Int32)
          , bench "Word32" $ nf (hash32Times 100) (maxBound :: Word32)
          , bench "Int64, in 32-bit range" $ nf (hash32Times 100) (maxBound :: Int64)
          , bench "Int64, out of 32-bit range" $ nf (hash32Times 100) ((fromIntegral (maxBound :: Int32) *2) :: Int64)
          , bench "Word64" $ nf (hash32Times 100) (maxBound :: Word64)
          , bench "Char, small" $ nf (hash32Times 100) 'a'
          , bench "Char, big"   $ nf (hash32Times 100) '\65537'
          , bench "Float" $ nf (hash32Times 100) (1.11111 :: Float)
          , bench "Double" $ nf (hash32Times 100) (1.111111111111 :: Double)
          , bench "Integer, in Int range" $ nf (hash32Times 100) (fromIntegral (maxBound :: Int) :: Integer)
          , bench "Integer, just out of Int range" $ nf (hash32Times 100) ((fromIntegral (maxBound :: Int) * 2) :: Integer)
          ]
      , bgroup "Sum types (of Word8, where relevant) x100" [
            bench "Bool (True)" $ nf (hash32Times 100) True
          , bench "Bool (False)" $ nf (hash32Times 100) False
          , bench "Ordering (LT)" $ nf (hash32Times 100) LT
          , bench "Ordering (EQ)" $ nf (hash32Times 100) EQ
          , bench "Ordering (GT)" $ nf (hash32Times 100) GT
          , bench "Maybe (Nothing)" $ nf (hash32Times 100) (Nothing :: Maybe Word8)
          , bench "Maybe (Just)" $ nf (hash32Times 100) (Just byt)
          , bench "Either (Left)" $ nf (hash32Times 100) (Left byt :: Either Word8 Word8)
          , bench "Either (Right)" $ nf (hash32Times 100) (Right byt :: Either Word8 Word8)
          ]
      , bgroup "Product types of Word8 x100" [
            bench "Ratio" $ nf (hash32Times 100) (0xFE % byt)
          , bench "(,)" $ nf (hash32Times 100) (byt,byt)
          , bench "(,,)" $ nf (hash32Times 100) (byt,byt,byt)
          , bench "(,,,)" $ nf (hash32Times 100) (byt,byt,byt,byt)
          , bench "(,,,,)" $ nf (hash32Times 100) (byt,byt,byt,byt,byt)
          , bench "(,,,,,)" $ nf (hash32Times 100) (byt,byt,byt,byt,byt,byt)
          , bench "(,,,,,,)" $ nf (hash32Times 100) (byt,byt,byt,byt,byt,byt,byt)
          , bench "(,,,,,,,)" $ nf (hash32Times 100) (byt,byt,byt,byt,byt,byt,byt,byt)
          ]

      , bgroup "on array types, hashFNV64" [
            bench "strict ByteString x50" $ nf hashFNV64 bs50
          -- ought to be same as above:
          , bench "trivial lazy ByteString x50" $ nf hashFNV64 bs50LazyTrivial
          , bench "Text x50" $ nf hashFNV64 t50
          -- ought to be same as above:
          , bench "trivial lazy Text x50" $ nf hashFNV64 t50LazyTrivial
          , bench "ByteArray x50" $ nf hashFNV64 ba50
          , bench "ByteArray x50 (pinned, aligned)" $ nf hashFNV64 ba50Aligned  -- (maybe non-aligned creation is aligned to word size)
          , bench "ByteArray x50 (pinned, incorrectly-aligned)" $ nf hashFNV64 ba50AlignedBadly  -- (no difference here either)

          , bench "ByteArray x1000" $ nf hashFNV64 ba1000
          , bench "strict ByteString x1000" $ nf hashFNV64 bs1000
          , bench "lazy ByteString x1000, in 20 chunks" $ nf hashFNV64 bs1000Lazy_by20Chunks
          , bench "Text x1000" $ nf hashFNV64 t1000
          , bench "lazy Text x1000, in 20 chunks" $ nf hashFNV64 t1000Lazy_by20Chunks
          -- TODO Integer of comparable size to above
          -- TODO BigNat on GHC 7.10
          -- TODO Natural on GHC 7.10
          ]

      , bgroup "on ByteStrings of various sizes, siphash64" [
          -- try to fool branch prediction in hashByteString and
          let ls = [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
           in bench "(baseline for fooling branch prediction)" $ nf (map (Hash64 :: Word64 -> Hash64 B.ByteString)) ls
          , bench "100 (fooling branch prediction)" $ nf
              (map (hashWord64 . siphash64 (SipKey 1 2)))
              [ bs1, bs9, bs8, bs32, bs11, bs9, bs3, bs16, bs11 ]
          , bench "96 (#1 more predictable branches)" $ nf
              (map (hashWord64 . siphash64 (SipKey 1 2)))
              [ bs16, bs16, bs16, bs16, bs16, bs16 ]
          , bench "96 (#2 more predictable branches)" $ nf
              (map (hashWord64 . siphash64 (SipKey 1 2)))
              [ bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8 ]

          , bench "1" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs1
          , bench "2" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs2
          , bench "3" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs3
          , bench "4" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs4
          , bench "5" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs5
          , bench "6" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs6
          , bench "7" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs7
          , bench "8" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs8
          , bench "16" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs16
          , bench "32" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs32
          , bench "64" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs64
          , bench "128" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs128
          , bench "256" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs256
          , bench "512" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs512
          , bench "1024" $ nf (hashWord64 . siphash64 (SipKey 1 2)) bs1024
      ]
      , bgroup "on ByteStrings of various sizes, hashable" [
          -- try to fool branch prediction in hashByteString and
          let ls = [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
           in bench "(baseline for fooling branch prediction)" $ nf (map (Hash64 :: Word64 -> Hash64 B.ByteString)) ls
          , bench "100 (fooling branch prediction)" $ nf
              (map Their.hash)
              [ bs1, bs9, bs8, bs32, bs11, bs9, bs3, bs16, bs11 ]
          , bench "96 (#1 more predictable branches)" $ nf
              (map Their.hash)
              [ bs16, bs16, bs16, bs16, bs16, bs16 ]
          , bench "96 (#2 more predictable branches)" $ nf
              (map Their.hash)
              [ bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8, bs8 ]

          , bench "1" $ nf Their.hash bs1
          , bench "2" $ nf Their.hash bs2
          , bench "3" $ nf Their.hash bs3
          , bench "4" $ nf Their.hash bs4
          , bench "5" $ nf Their.hash bs5
          , bench "6" $ nf Their.hash bs6
          , bench "7" $ nf Their.hash bs7
          , bench "8" $ nf Their.hash bs8
          , bench "16" $ nf Their.hash bs16
          , bench "32" $ nf Their.hash bs32
          , bench "64" $ nf Their.hash bs64
          , bench "128" $ nf Their.hash bs128
          , bench "256" $ nf Their.hash bs256
          , bench "512" $ nf Their.hash bs512
          , bench "1024" $ nf Their.hash bs1024
      ]
      , bgroup "on array types, siphash64" [
            bench "strict ByteString x50" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) bs50
          -- ought to be same as above:
          , bench "trivial lazy ByteString x50" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) bs50LazyTrivial
          , bench "Text x50" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) t50
          -- ought to be same as above:
          , bench "trivial lazy Text x50" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) t50LazyTrivial
          , bench "ByteArray x50" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) ba50
          , bench "ByteArray x50 (pinned, aligned)" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) ba50Aligned  -- (maybe non-aligned creation is aligned to word size)
          , bench "ByteArray x50 (pinned, incorrectly-aligned)" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) ba50AlignedBadly  -- (no difference here either)

          , bench "ByteArray x1000" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) ba1000
          , bench "strict ByteString x1000" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) bs1000
          , bench "lazy ByteString x1000, in 20 chunks" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) bs1000Lazy_by20Chunks
          , bench "Text x1000" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) t1000
          , bench "lazy Text x1000, in 20 chunks" $ nf (hashWord64 . siphash64 (SipKey 0x0706050403020100 0x0F0E0D0C0B0A0908)) t1000Lazy_by20Chunks
          -- TODO Integer of comparable size to above
          -- TODO BigNat on GHC 7.10
          -- TODO Natural on GHC 7.10
          ]

        
      , listBgroup "medium-size lists" 250
      -- In line with above, although NoList variants win out by a greater margin:
      --, listBgroup "small lists" 5

      ]
     ]



-- --------------------------------------------------------
-- Sorry, couldn't figure out how to turn these into CPP macros:


-- Taken from 'hashable' Benchmarks.hs @47eaf9f:
hashableBenchmarkFNV64 :: Benchmark
{-# INLINE hashableBenchmarkFNV64 #-}
hashableBenchmarkFNV64 =
    let
        !mb = (2::Int)^(20 :: Int)  -- 1 Mb
        s5 = ['\0'..'\4'];   s8 = ['\0'..'\7'];     s11 = ['\0'..'\10']
        s40 = ['\0'..'\39']; s128 = ['\0'..'\127']; s512 = ['\0'..'\511']
        s1Mb = ['\0'..'\999999']

        !bs5 = B8.pack s5;   !bs8 = B8.pack s8;     !bs11 = B8.pack s11
        !bs40 = B8.pack s40; !bs128 = B8.pack s128; !bs512 = B8.pack s512
        !bs1Mb = B8.pack s1Mb

        blmeg = BL.take (fromIntegral mb) . BL.fromChunks . repeat
        bl5 = BL.fromChunks [bs5];     bl8 = BL.fromChunks [bs8]
        bl11 = BL.fromChunks [bs11];   bl40 = BL.fromChunks [bs40]
        bl128 = BL.fromChunks [bs128]; bl512 = BL.fromChunks [bs512]
        bl1Mb_40 = blmeg bs40;         bl1Mb_128 = blmeg bs128
        bl1Mb_64k = blmeg (B8.take 65536 bs1Mb)

        !t5 = T.pack s5;   !t8 = T.pack s8;     !t11 = T.pack s11
        !t40 = T.pack s40; !t128 = T.pack s128; !t512 = T.pack s512
        !t1Mb = T.pack s1Mb

        tlmeg = TL.take (fromIntegral mb) . TL.fromChunks . repeat
        tl5 = TL.fromStrict t5;     tl8 = TL.fromStrict t8
        tl11 = TL.fromStrict t11;   tl40 = TL.fromStrict t40
        tl128 = TL.fromStrict t128; tl512 = TL.fromChunks (replicate 4 t128)
        tl1Mb_40 = tlmeg t40;       tl1Mb_128 = tlmeg t128
        tl1Mb_64k = tlmeg (T.take 65536 t1Mb)
     in bgroup "hashabler hashFNV64"
          [ bgroup "ByteString"
            [ bgroup "strict"
              [ bench "5" $ whnf (hashWord64 . hashFNV64) bs5
              , bench "8" $ whnf (hashWord64 . hashFNV64) bs8
              , bench "11" $ whnf (hashWord64 . hashFNV64) bs11
              , bench "40" $ whnf (hashWord64 . hashFNV64) bs40
              , bench "128" $ whnf (hashWord64 . hashFNV64) bs128
              , bench "512" $ whnf (hashWord64 . hashFNV64) bs512
              , bench "2^20" $ whnf (hashWord64 . hashFNV64) bs1Mb
              ]
            , bgroup "lazy"
                [ bench "5" $ whnf (hashWord64 . hashFNV64) bl5
                , bench "8" $ whnf (hashWord64 . hashFNV64) bl8
                , bench "11" $ whnf (hashWord64 . hashFNV64) bl11
                , bench "40" $ whnf (hashWord64 . hashFNV64) bl40
                , bench "128" $ whnf (hashWord64 . hashFNV64) bl128
                , bench "512" $ whnf (hashWord64 . hashFNV64) bl512
                , bench "2^20_40" $ whnf (hashWord64 . hashFNV64) bl1Mb_40
                , bench "2^20_128" $ whnf (hashWord64 . hashFNV64) bl1Mb_128
                , bench "2^20_64k" $ whnf (hashWord64 . hashFNV64) bl1Mb_64k
                ]
            ]
          , bgroup "String"
            [ bench "5" $ whnf (hashWord64 . hashFNV64) s5
            , bench "8" $ whnf (hashWord64 . hashFNV64) s8
            , bench "11" $ whnf (hashWord64 . hashFNV64) s11
            , bench "40" $ whnf (hashWord64 . hashFNV64) s40
            , bench "128" $ whnf (hashWord64 . hashFNV64) s128
            , bench "512" $ whnf (hashWord64 . hashFNV64) s512
            , bench "2^20" $ whnf (hashWord64 . hashFNV64) s1Mb
            ]
          , bgroup "Text"
            [ bgroup "strict"
              [ bench "5" $ whnf (hashWord64 . hashFNV64) t5
              , bench "8" $ whnf (hashWord64 . hashFNV64) t8
              , bench "11" $ whnf (hashWord64 . hashFNV64) t11
              , bench "40" $ whnf (hashWord64 . hashFNV64) t40
              , bench "128" $ whnf (hashWord64 . hashFNV64) t128
              , bench "512" $ whnf (hashWord64 . hashFNV64) t512
              , bench "2^20" $ whnf (hashWord64 . hashFNV64) t1Mb
              ]
            , bgroup "lazy"
              [ bench "5" $ whnf (hashWord64 . hashFNV64) tl5
              , bench "8" $ whnf (hashWord64 . hashFNV64) tl8
              , bench "11" $ whnf (hashWord64 . hashFNV64) tl11
              , bench "40" $ whnf (hashWord64 . hashFNV64) tl40
              , bench "128" $ whnf (hashWord64 . hashFNV64) tl128
              , bench "512" $ whnf (hashWord64 . hashFNV64) tl512
              , bench "2^20_40" $ whnf (hashWord64 . hashFNV64) tl1Mb_40
              , bench "2^20_128" $ whnf (hashWord64 . hashFNV64) tl1Mb_128
              , bench "2^20_64k" $ whnf (hashWord64 . hashFNV64) tl1Mb_64k
              ]
            ]
          , bench "Int8" $ whnf (hashWord64 . hashFNV64) (127 :: Int8)
          , bench "Int16" $ whnf (hashWord64 . hashFNV64) (0x7eef :: Int16)
          , bench "Int32" $ whnf (hashWord64 . hashFNV64) (0x7eadbeef :: Int32)
          , bench "Int" $ whnf (hashWord64 . hashFNV64) (0x7eadbeefdeadbeef :: Int)
          , bench "Int64" $ whnf (hashWord64 . hashFNV64) (0x7eadbeefdeadbeef :: Int64)
          , bench "Double" $ whnf (hashWord64 . hashFNV64) (0.3780675796601578 :: Double)
          ]

-- Taken from 'hashable' Benchmarks.hs @47eaf9f:
hashableBenchmarkSiphash64 :: Benchmark
{-# INLINE hashableBenchmarkSiphash64 #-}
hashableBenchmarkSiphash64 =
    let
        !mb = (2::Int)^(20 :: Int)  -- 1 Mb
        s5 = ['\0'..'\4'];   s8 = ['\0'..'\7'];     s11 = ['\0'..'\10']
        s40 = ['\0'..'\39']; s128 = ['\0'..'\127']; s512 = ['\0'..'\511']
        s1Mb = ['\0'..'\999999']

        !bs5 = B8.pack s5;   !bs8 = B8.pack s8;     !bs11 = B8.pack s11
        !bs40 = B8.pack s40; !bs128 = B8.pack s128; !bs512 = B8.pack s512
        !bs1Mb = B8.pack s1Mb

        blmeg = BL.take (fromIntegral mb) . BL.fromChunks . repeat
        bl5 = BL.fromChunks [bs5];     bl8 = BL.fromChunks [bs8]
        bl11 = BL.fromChunks [bs11];   bl40 = BL.fromChunks [bs40]
        bl128 = BL.fromChunks [bs128]; bl512 = BL.fromChunks [bs512]
        bl1Mb_40 = blmeg bs40;         bl1Mb_128 = blmeg bs128
        bl1Mb_64k = blmeg (B8.take 65536 bs1Mb)

        !t5 = T.pack s5;   !t8 = T.pack s8;     !t11 = T.pack s11
        !t40 = T.pack s40; !t128 = T.pack s128; !t512 = T.pack s512
        !t1Mb = T.pack s1Mb

        tlmeg = TL.take (fromIntegral mb) . TL.fromChunks . repeat
        tl5 = TL.fromStrict t5;     tl8 = TL.fromStrict t8
        tl11 = TL.fromStrict t11;   tl40 = TL.fromStrict t40
        tl128 = TL.fromStrict t128; tl512 = TL.fromChunks (replicate 4 t128)
        tl1Mb_40 = tlmeg t40;       tl1Mb_128 = tlmeg t128
        tl1Mb_64k = tlmeg (T.take 65536 t1Mb)
     in bgroup "hashabler siphash64"
          [ bgroup "ByteString"
            [ bgroup "strict"
              [ bench "5" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs5
              , bench "8" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs8
              , bench "11" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs11
              , bench "40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs40
              , bench "128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs128
              , bench "512" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs512
              , bench "2^20" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bs1Mb
              ]
            , bgroup "strict (with siphash-1-3)" -- TODO make proper benchmark set for siphash64_1_3
              [ bench "5" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs5
              , bench "8" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs8
              , bench "11" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs11
              , bench "40" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs40
              , bench "128" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs128
              , bench "512" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs512
              , bench "2^20" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) bs1Mb
              ]
            , bgroup "lazy"
                [ bench "5" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl5
                , bench "8" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl8
                , bench "11" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl11
                , bench "40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl40
                , bench "128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl128
                , bench "512" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl512
                , bench "2^20_40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl1Mb_40
                , bench "2^20_128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl1Mb_128
                , bench "2^20_64k" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) bl1Mb_64k
                ]
            ]
          , bgroup "String"
            [ bench "5" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s5
            , bench "8" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s8
            , bench "11" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s11
            , bench "40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s40
            , bench "128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s128
            , bench "512" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s512
            , bench "2^20" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) s1Mb
            ]
          , bgroup "Text"
            [ bgroup "strict"
              [ bench "5" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t5
              , bench "8" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t8
              , bench "11" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t11
              , bench "40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t40
              , bench "128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t128
              , bench "512" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t512
              , bench "2^20" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) t1Mb
              ]
            , bgroup "strict (with siphash-1-3)" -- TODO make proper benchmark set for siphash64_1_3
              [ bench "5" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t5
              , bench "8" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t8
              , bench "11" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t11
              , bench "40" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t40
              , bench "128" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t128
              , bench "512" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t512
              , bench "2^20" $ whnf (hashWord64 . siphash64_1_3 (SipKey 1 2)) t1Mb
              ]
            , bgroup "lazy"
              [ bench "5" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl5
              , bench "8" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl8
              , bench "11" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl11
              , bench "40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl40
              , bench "128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl128
              , bench "512" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl512
              , bench "2^20_40" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl1Mb_40
              , bench "2^20_128" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl1Mb_128
              , bench "2^20_64k" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) tl1Mb_64k
              ]
            ]
          , bench "Int8" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) (127 :: Int8)
          , bench "Int16" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) (0x7eef :: Int16)
          , bench "Int32" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) (0x7eadbeef :: Int32)
          , bench "Int" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) (0x7eadbeefdeadbeef :: Int)
          , bench "Int64" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) (0x7eadbeefdeadbeef :: Int64)
          , bench "Double" $ whnf (hashWord64 . siphash64 (SipKey 1 2)) (0.3780675796601578 :: Double)
          ]


-- Same as above; just couldn't quite figure out how to get this to work
hashableBenchmarkTheir :: String -> Benchmark
{-# INLINE hashableBenchmarkTheir #-}
hashableBenchmarkTheir nm =
    let
        !mb = (2::Int)^(20 :: Int)  -- 1 Mb
        s5 = ['\0'..'\4'];   s8 = ['\0'..'\7'];     s11 = ['\0'..'\10']
        s40 = ['\0'..'\39']; s128 = ['\0'..'\127']; s512 = ['\0'..'\511']
        s1Mb = ['\0'..'\999999']

        !bs5 = B8.pack s5;   !bs8 = B8.pack s8;     !bs11 = B8.pack s11
        !bs40 = B8.pack s40; !bs128 = B8.pack s128; !bs512 = B8.pack s512
        !bs1Mb = B8.pack s1Mb

        blmeg = BL.take (fromIntegral mb) . BL.fromChunks . repeat
        bl5 = BL.fromChunks [bs5];     bl8 = BL.fromChunks [bs8]
        bl11 = BL.fromChunks [bs11];   bl40 = BL.fromChunks [bs40]
        bl128 = BL.fromChunks [bs128]; bl512 = BL.fromChunks [bs512]
        bl1Mb_40 = blmeg bs40;         bl1Mb_128 = blmeg bs128
        bl1Mb_64k = blmeg (B8.take 65536 bs1Mb)

        !t5 = T.pack s5;   !t8 = T.pack s8;     !t11 = T.pack s11
        !t40 = T.pack s40; !t128 = T.pack s128; !t512 = T.pack s512
        !t1Mb = T.pack s1Mb

        tlmeg = TL.take (fromIntegral mb) . TL.fromChunks . repeat
        tl5 = TL.fromStrict t5;     tl8 = TL.fromStrict t8
        tl11 = TL.fromStrict t11;   tl40 = TL.fromStrict t40
        tl128 = TL.fromStrict t128; tl512 = TL.fromChunks (replicate 4 t128)
        tl1Mb_40 = tlmeg t40;       tl1Mb_128 = tlmeg t128
        tl1Mb_64k = tlmeg (T.take 65536 t1Mb)
     in bgroup nm
          [ bgroup "ByteString"
            [ bgroup "strict"
              [ bench "5" $ whnf Their.hash bs5
              , bench "8" $ whnf Their.hash bs8
              , bench "11" $ whnf Their.hash bs11
              , bench "40" $ whnf Their.hash bs40
              , bench "128" $ whnf Their.hash bs128
              , bench "512" $ whnf Their.hash bs512
              , bench "2^20" $ whnf Their.hash bs1Mb
              ]
            , bgroup "lazy"
                [ bench "5" $ whnf Their.hash bl5
                , bench "8" $ whnf Their.hash bl8
                , bench "11" $ whnf Their.hash bl11
                , bench "40" $ whnf Their.hash bl40
                , bench "128" $ whnf Their.hash bl128
                , bench "512" $ whnf Their.hash bl512
                , bench "2^20_40" $ whnf Their.hash bl1Mb_40
                , bench "2^20_128" $ whnf Their.hash bl1Mb_128
                , bench "2^20_64k" $ whnf Their.hash bl1Mb_64k
                ]
            ]
          , bgroup "String"
            [ bench "5" $ whnf Their.hash s5
            , bench "8" $ whnf Their.hash s8
            , bench "11" $ whnf Their.hash s11
            , bench "40" $ whnf Their.hash s40
            , bench "128" $ whnf Their.hash s128
            , bench "512" $ whnf Their.hash s512
            , bench "2^20" $ whnf Their.hash s1Mb
            ]
          , bgroup "Text"
            [ bgroup "strict"
              [ bench "5" $ whnf Their.hash t5
              , bench "8" $ whnf Their.hash t8
              , bench "11" $ whnf Their.hash t11
              , bench "40" $ whnf Their.hash t40
              , bench "128" $ whnf Their.hash t128
              , bench "512" $ whnf Their.hash t512
              , bench "2^20" $ whnf Their.hash t1Mb
              ]
            , bgroup "lazy"
              [ bench "5" $ whnf Their.hash tl5
              , bench "8" $ whnf Their.hash tl8
              , bench "11" $ whnf Their.hash tl11
              , bench "40" $ whnf Their.hash tl40
              , bench "128" $ whnf Their.hash tl128
              , bench "512" $ whnf Their.hash tl512
              , bench "2^20_40" $ whnf Their.hash tl1Mb_40
              , bench "2^20_128" $ whnf Their.hash tl1Mb_128
              , bench "2^20_64k" $ whnf Their.hash tl1Mb_64k
              ]
            ]
          , bench "Int8" $ whnf Their.hash (127 :: Int8)
          , bench "Int16" $ whnf Their.hash (0x7eef :: Int16)
          , bench "Int32" $ whnf Their.hash (0x7eadbeef :: Int32)
          , bench "Int" $ whnf Their.hash (0x7eadbeefdeadbeef :: Int)
          , bench "Int64" $ whnf Their.hash (0x7eadbeefdeadbeef :: Int64)
          , bench "Double" $ whnf Their.hash (0.3780675796601578 :: Double)
          ]
