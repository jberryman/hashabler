{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( 
    main
   ) where

import Criterion.Main
import Data.Word
import Data.Hashable.FNV1a
import Data.Int
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Primitive as P



-- BASELINE for list instances: 
-- a fused foldl' equivalent -- NOTE ~ 2x faster than Unfolded on 7.10
hashLeftNoList :: Word32 -> Word8 -> Word32  -- NOTE: tested w/ this monomorphic sig
{-# INLINE hashLeftNoList #-}
hashLeftNoList = go
    where go !h 0 = h
          go !h !b = go (h `hash32WithSalt` b) (b-1)



main :: IO ()
main = do
    let bs50 = B.pack $ replicate 50 1
        bs1000 = B.pack $ replicate 1000 1
        t50 = T.pack $ replicate 25 'a' -- TODO verify this is 50 bytes
        t1000 = T.pack $ replicate 500 'a' -- TODO verify this is 1000 bytes
    ba50 <- P.newByteArray 50 >>= \ba'-> P.fillByteArray ba' 0 50 1 >> P.unsafeFreezeByteArray ba'
    -- lazy Text and ByteString:
    let bs50LazyTrivial = BL.fromStrict bs50
        bs1000Lazy_by20Chunks = BL.fromChunks $ replicate 20 bs50
    let t50LazyTrivial = TL.fromStrict t50
        t1000Lazy_by20Chunks = TL.fromChunks $ replicate 20 t50

    let char2Word :: Char -> Word16
        char2Word = fromIntegral . ord


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
    defaultMain [ 
        -- We can more or less subtract this from benchmarks producing a Word32 hash:

        bench "baseline Word32" $ nf (\x-> x) (777::Word32)
      , bench "hash32 (Int)" $ nf hash32 (9999::Int)
      , bench "hash32 (Int_64_internal, in 32-bit range)" $ nf hash32 (9999 :: Int)
      , bench "hash32 (Int_64_internal, out of 32-bit range)" $ nf (_hash32WithSalt_Int_64 fnvOffsetBasis32) (fromIntegral (maxBound :: Int32) *2)

      , bench "hash32 via ord (Char small)" $ nf (hash32 . char2Word) 'a'
      , bench "hash32 (Char small)" $ nf hash32 'a'
      , bench "hash32 (Char big)"   $ nf hash32 '\65537'

      , bench "hash32 (strict ByteString x50)" $ nf hash32 bs50 
      , bench "hash32 (strict ByteString x1000)" $ nf hash32 bs1000
      -- ought to be same as above:
      , bench "hash32 (trivial lazy ByteString x50)" $ nf hash32 bs50LazyTrivial
      , bench "hash32 (lazy ByteString x1000, in 20 chunks)" $ nf hash32 bs1000Lazy_by20Chunks

      , bench "hash32 (Text x50)" $ nf hash32 t50
      , bench "hash32 (Text x1000)" $ nf hash32 t1000
      , bench "hash32 (trivial lazy Text x50)" $ nf hash32 t50LazyTrivial
      -- ought to be same as above:
      , bench "hash32 (ByteArray x50)" $ nf hash32 ba50
      , bench "hash32 (lazy Text x1000, in 20 chunks)" $ nf hash32 t1000Lazy_by20Chunks


      , bench "hash32 (Float)" $ nf hash32 (1.11111 :: Float)
      , bench "hash32 (Double)" $ nf hash32 (1.111111111111 :: Double)
      

      , bench "hash32 (Word16)" $ nf hash32 (0x6666 :: Word16)
      , bench "hash32 (Word32)" $ nf hash32 (0x66666666 :: Word32)
        
      , listBgroup "medium-size lists" 250
      -- In line with above, although NoList variants win out by a greater margin:
      --, listBgroup "small lists" 5

      {- Conversion internals
      , bench "bytes32" $ nf bytes32 0x66666666
      , bench "bytesFloat" $ nf bytesFloat (1.11111)           -- 24ns
      , bench "bytesDouble" $ nf bytesDouble (1.111111111111)  -- 33ns
      , bench "baseline bytes (deepseq'd)" $ nf (\x-> x) bytes4
      , bench "floatToWord" $ nf floatToWord 1.11111          -- 24ns
      , bench "baseline Word32" $ nf (\x-> x) (111 :: Word32)
      , bench "doubleToWord" $ nf doubleToWord 1.111111111111 -- 24ns
      , bench "baseline Word64" $ nf (\x-> x)  (111 :: Word64)
      -}
      ]
