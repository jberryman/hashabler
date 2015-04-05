{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric,StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main ( 
    main
   ) where

import Criterion.Main
import Data.Word
import Data.Hashable.FNV1a
import Data.Int
import Data.Ratio
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Primitive as P
import Control.DeepSeq

instance NFData FNV32 where rnf = rnf . fnv32 

-- BASELINE for list instances: 
-- a fused foldl' equivalent -- NOTE ~ 2x faster than Unfolded on 7.10
hashLeftNoList :: FNV32 -> Word8 -> Word32  -- NOTE: tested w/ this monomorphic sig
{-# INLINE hashLeftNoList #-}
hashLeftNoList = go
    where go !h 0 = fnv32 h
          go !h !b = go (h `hash` b) (b-1)

hash32Times :: Hashable a=> Int -> a -> Word32
{-# INLINE hash32Times #-}
hash32Times iters =
  \a->  let go !h !0 = h
            go !h !n = go (h `hash` a) (n-1)
         in fnv32 $ go fnvOffsetBasis32 iters 

main :: IO ()
main = do
    let bs50 = B.pack $ replicate 50 1
        bs1000 = B.pack $ replicate 1000 1
        t50 = T.pack $ replicate 25 'a' -- TODO verify this is 50 bytes
        t1000 = T.pack $ replicate 500 'a' -- TODO verify this is 1000 bytes
    ba50 <- P.newByteArray 50 >>= \ba'-> P.fillByteArray ba' 0 50 1 >> P.unsafeFreezeByteArray ba'
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
    defaultMain [ 
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

      , bgroup "hashFNV32 on array types" [
            bench "strict ByteString x50" $ nf hashFNV32 bs50 
          -- ought to be same as above:
          , bench "trivial lazy ByteString x50" $ nf hashFNV32 bs50LazyTrivial
          , bench "Text x50" $ nf hashFNV32 t50
          -- ought to be same as above:
          , bench "trivial lazy Text x50" $ nf hashFNV32 t50LazyTrivial
          , bench "ByteArray x50" $ nf hashFNV32 ba50

          , bench "ByteArray x1000" $ nf hashFNV32 ba1000
          , bench "strict ByteString x1000" $ nf hashFNV32 bs1000
          , bench "lazy ByteString x1000, in 20 chunks" $ nf hashFNV32 bs1000Lazy_by20Chunks
          , bench "Text x1000" $ nf hashFNV32 t1000
          , bench "lazy Text x1000, in 20 chunks" $ nf hashFNV32 t1000Lazy_by20Chunks
          -- TODO Integer of comparable size to above
          -- TODO BigNat on GHC 7.10
          -- TODO Natural on GHC 7.10
          ]

        
      , listBgroup "medium-size lists" 250
      -- In line with above, although NoList variants win out by a greater margin:
      --, listBgroup "small lists" 5
      ]
