{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( 
    main
   ) where

import Criterion.Main
import Data.Word

import Data.Hashable.FNV1a

-- 
import Data.ByteString.Builder
import Data.Int

--
import Serialized

-- 
import Data.Bits
import Control.DeepSeq
import Data.LargeWord
import GHC.Generics
import Data.Char

import Foreign.Marshal.Utils
import Foreign.Storable

-- floating-bits "does not really work"
-- import Data.Bits.Floating

import Data.List

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

instance (NFData a, NFData b)=> NFData (LargeKey a b)
deriving instance Generic (LargeKey a b)

{-
-- Stupid, but for our information
bytes :: Word -> (Word8,Word8,Word8,Word8)
bytes !wd = (shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd
     -}

-- for sanity testing timing of above:
xor4 :: (Word8,Word8,Word8,Word8) -> Word8
xor4 (a,b,c,d) = a `xor` b `xor` c `xor` d `xor` a `xor` b `xor` c `xor` d 



main :: IO ()
main = do
    let lbs32 = toLazyByteString . int32LE $ (777::Int32)
        lbs64 = toLazyByteString . int64LE $ (777::Int64)
    let !bytes4 = force (3::Word8,4::Word8,5::Word8,6::Word8)

    let to128 :: Word -> Word128
        to128 = fromIntegral
        !w128 = force maxBound :: Word128
        !w128prim = force $ fromIntegral (309485009821345068724781371::Integer) :: Word128
    let to64 :: Word -> Word64
        to64 = fromIntegral
        !w64 = force maxBound :: Word64
        !w64prime = force 1099511628211 :: Word64

    let bs50 = B.pack $ replicate 50 1
    let bs1000 = B.pack $ replicate 1000 1

    -- LISTS INSTANCES SCRATCH:
      -- Significantly slower than unfolded:
      -- , bench "hashFoldl'" $ nf (hashFoldl' fnvOffsetBasis32) [1..250]
      -- , bench "hashFoldl'Extra" $ nf (hashFoldl'Extra fnvOffsetBasis32) [1..250]
      -- , bench "hashFoldr" $ nf (hashFoldr fnvOffsetBasis32) [1..250]
      -- , bench "hashFoldrExtra" $ nf (hashFoldrExtra fnvOffsetBasis32) [1..250]

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
            , bench "hashFoldl' trying to fuse" $ nf (\i-> hashFoldl' fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) 1
            -- adding extra multiply adds ~ 65% overhead!:
          --, bench "hashFoldl'Extra trying to fuse" $ nf (\i-> hashFoldl'Extra fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) 1
            -- Faster than un-fused (I think that's what's happening) fold-based code:
            , bench "hashLeftUnfolded" $ nf (hashLeftUnfolded fnvOffsetBasis32) [1..sz]
          --, bench "hashLeftUnfolded trying to fuse" $ nf (\i-> hashLeftUnfolded fnvOffsetBasis32 (take (fromIntegral sz) $ iterate (+1) i)) 1
            ]
    let bgroupBytestring sz bs = bgroup ("bytestrings "++show sz) $ [
                bench "hashBytesEach 50" $ nfIO $ hashBytesEach fnvOffsetBasis32 bs 
              , bench "hashBytesWord32 50" $ nfIO $ hashBytesWord32 fnvOffsetBasis32 bs 
              , bench "hashBytesWord32x2 50" $ nfIO $ hashBytesWord32x2 fnvOffsetBasis32 bs 
              {- Approx the same:
              , bench "inlinePerformIO hashBytesEach 50" $ nf (B.inlinePerformIO . hashBytesEach fnvOffsetBasis32) bs 
              , bench "inlinePerformIO hashBytesWord32 50" $ nf (B.inlinePerformIO . hashBytesWord32 fnvOffsetBasis32) bs 
              , bench "inlinePerformIO hashBytesWord32x2 50" $ nf (B.inlinePerformIO . hashBytesWord32x2 fnvOffsetBasis32) bs 
              -}
              ]
    defaultMain [ 
        bgroupBytestring 50 bs50
      , bgroupBytestring 1000 bs1000

      , bench "bytes32" $ nf bytes32 0x66666666

      , bench "hash32 (Float)" $ nf hash32 (1.11111 :: Float)
      , bench "hash32 (Double)" $ nf hash32 (1.111111111111 :: Double)
      , bench "bytesFloat" $ nf bytesFloat (1.11111)
      , bench "bytesDouble" $ nf bytesDouble (1.111111111111)
      , bench "baseline bytes (deepseq'd)" $ nf (\x-> x) bytes4

      , bench "hash32 (Word16)" $ nf hash32 (0x6666 :: Word16)
      , bench "testWord w/ helper 32" $ nf hash32Word32 0x66666666
      , bench "hash32 (Word32)" $ nf hash32 (0x66666666 :: Word32)
        
      , listBgroup "medium-size lists" 250
      -- In line with above, although NoList variants win out by a greater margin:
      --, listBgroup "small lists" 5


      , bench "testWord w/ helper 32" $ nf hash32Word32 0x66666666
      , bench "bytes64" $ nf bytes64 0x66666666
      , bench "bytes64_alt" $ nf bytes64_alt 0x66666666
      , bench "testWord w/ helper 64" $ nf hash32Word64 0x6666666666666666
      , bench "test ord Char" $ nf ord maxBound
      , bench "Word8 roundtrip alloca/peek" $ nfIO $ with (0::Word8)  peek

      , bench "floatToWord" $ nf floatToWord 1.11111
      , bench "baseline Word32" $ nf (\x-> x) (111 :: Word32)
      , bench "doubleToWord" $ nf doubleToWord 1.111111111111
      , bench "baseline Word64" $ nf (\x-> x)  (111 :: Word64)

      {- Also really fucking slow
      , bench "decodeFloat" $ nf decodeFloat (1.11111 :: Float)
      , bench "baseline decodeFloat" $ nf (\x-> x) (9320666::Integer,-23::Int)
      -}

      , bench "baseline32" $ nf (\x-> x) (777::Word32)

      {- quite slow
      , bench "bytestring 32" $ nf (toLazyByteString . int32LE) (777::Int32)
      , bench "bytestring 64" $ nf (toLazyByteString . int64LE) (777::Int64)
      , bench "baseline lbs32" $ nf (\x-> x) lbs32
      , bench "baseline lbs64" $ nf (\x-> x) lbs64
      -}

      {- super slow
      , bench "Serialized Word" $ nf serializeWithData (2::Word) 
      , bench "baseline Serialized" $ nf (\x-> x) ([2,1,0,0,0,50,0,0,0]::[Word8])
      -}
      {- GOOD
      , bench "bytes" $ nf bytes 777
      , bench "xor4" $ nf xor4 (3::Word8,4::Word8,5::Word8,6::Word8)
      , bench "xor4 (deepseq'd)" $ nf xor4 bytes4
      , bench "xor4 . bytes" $ nf (xor4 . bytes) (4294900000)
      , bench "baseline bytes" $ nf (\x-> x) (3::Word8,4::Word8,5::Word8,6::Word8)
      , bench "baseline bytes (deepseq'd)" $ nf (\x-> x) bytes4
      -}
      {- largewords extremely slow, 64bit multiply on 32bit is slow
      , bench "128 fromIntegral" $ nf to128 111
      , bench "128 fromIntegral xor" $ nf (xor w128 . to128) 111
      , bench "128 fromIntegral xor * prime" $ nf ((* w128prim) . xor w128 . to128) 111
      , bench "baseline w128" $ nf (\x-> x) w128
      
      , bench "64 fromIntegral" $ nf to64 111
      , bench "64 fromIntegral xor" $ nf (xor w64 . to64) 111
      , bench "64 fromIntegral xor * prime" $ nf ((* w64prime) . xor w64 . to64) 111
      , bench "baseline w64" $ nf (\x-> x) w64
      -}
      ]

