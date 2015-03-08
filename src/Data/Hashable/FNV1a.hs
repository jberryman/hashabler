{-# LANGUAGE BangPatterns #-}
module Data.Hashable.FNV1a 
    where
    
import Data.Word
import Data.Bits

{-
hash = offset_basis
for each octet_of_data to be hashed
        hash = hash xor octet_of_data
        hash = hash * FNV_prime
return hash


-- see also the non-powers of two mapping methods outlined:
--  http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1a

For test vectors:
    http://www.isthe.com/chongo/src/fnv/test_fnv.c
-}

fnvPrime32 :: Word32
{-# INLINE fnvPrime32 #-}
fnvPrime32 = 16777619

fnvPrime64 :: Word64
{-# INLINE fnvPrime64 #-}
fnvPrime64 = 1099511628211
-- fnvPrime128 = 309485009821345068724781371
-- fnvPrime256 = 374144419156711147060143317175368453031918731002211

fnvOffsetBasis32 :: Word32
{-# INLINE fnvOffsetBasis32 #-}
fnvOffsetBasis32 = 2166136261

fnvOffsetBasis64 :: Word64
{-# INLINE fnvOffsetBasis64 #-}
fnvOffsetBasis64 = 14695981039346656037
-- fnvOffsetBasis128 = 144066263297769815596495629667062367629
-- fnvOffsetBasis256 = 100029257958052580907070968620625704837092796014241193945225284501741471925557

-- TODO remove:
fnvInnerLoopTest :: Word8 -> Word32
{-# INLINE fnvInnerLoopTest #-}
fnvInnerLoopTest b = (fnvOffsetBasis32 `xor` fromIntegral b) * fnvPrime32


-- TODO remove:
fnvInnerLoopTest4 :: (Word8 , Word8 , Word8 , Word8) -> Word32
{-# INLINE fnvInnerLoopTest4 #-}
fnvInnerLoopTest4 (b0,b1,b2,b3) = 
    ((((((((fnvOffsetBasis32 `xor` fromIntegral b0) * fnvPrime32)
    `xor` fromIntegral b1) * fnvPrime32)
    `xor` fromIntegral b2) * fnvPrime32)
    `xor` fromIntegral b3) * fnvPrime32)

-- UNROLLED 32 BIT HASHES:
--    TODO bench alongside a <## operator, inlined
fnvInnerLoopTestWord :: Word32 -> Word32
fnvInnerLoopTestWord wd = case bytes32 wd of 
  (b0,b1,b2,b3)->
    ((((((((fnvOffsetBasis32 `xor` fromIntegral b0) * fnvPrime32)
    `xor` fromIntegral b1) * fnvPrime32)
    `xor` fromIntegral b2) * fnvPrime32)
    `xor` fromIntegral b3) * fnvPrime32)

fnvInnerLoopTestWord64 :: Word64 -> Word32
{-# INLINE fnvInnerLoopTestWord64 #-}
fnvInnerLoopTestWord64 wd = case bytes64 wd of
  (b0,b1,b2,b3,b4,b5,b6,b7) ->
    ((((((((((((((((fnvOffsetBasis32 `xor` fromIntegral b0) * fnvPrime32)
    `xor` fromIntegral b1) * fnvPrime32)
    `xor` fromIntegral b2) * fnvPrime32)
    `xor` fromIntegral b3) * fnvPrime32)
    `xor` fromIntegral b4) * fnvPrime32)
    `xor` fromIntegral b5) * fnvPrime32)
    `xor` fromIntegral b6) * fnvPrime32)
    `xor` fromIntegral b7) * fnvPrime32)


-- NOTE we're to hash these Word8s from left to right
bytes32 :: Word32 -> (Word8,Word8,Word8,Word8)
{-# INLINE bytes32 #-}
bytes32 wd = (shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

bytes64 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytes64 #-}
bytes64 wd = ( shifted 56, shifted 48, shifted 40, shifted 32
             , shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd



-- TODO remove
fnvInnerLoopTest8 :: (Word8 , Word8 , Word8 , Word8,Word8 , Word8 , Word8 , Word8) -> Word32
{-# INLINE fnvInnerLoopTest8 #-}
fnvInnerLoopTest8 (b0,b1,b2,b3,b4,b5,b6,b7) = 
    ((((((((((((((((fnvOffsetBasis32 `xor` fromIntegral b0) * fnvPrime32)
    `xor` fromIntegral b1) * fnvPrime32)
    `xor` fromIntegral b2) * fnvPrime32)
    `xor` fromIntegral b3) * fnvPrime32)
    `xor` fromIntegral b4) * fnvPrime32)
    `xor` fromIntegral b5) * fnvPrime32)
    `xor` fromIntegral b6) * fnvPrime32)
    `xor` fromIntegral b7) * fnvPrime32)


-- NOTES:
--   - no overhead from fromIntegral
--              1 iteration  =  9.37 (+0)
--              4 iterations = 12.66 (+3.3)  0.8
--              8 iterations = 17.83 (+8.5)  1.1
--   - baseline for id = 9.494
--   - using Word instead of Word32 explicitly doesn't matter
--   - using all Int and no fromIntegrals doesn't matter
--   - Compiling w/ LLVM was no better.
--   - using unsafeShiftR to get bytes is fast and probably our best choice
--   - largeword is much too slow to be usable
--   - 64-bit multiplication is slow on 32-bit (overhead of ~10ns)
--
-- CROSS-PLATFORM NOTES:
--   - Int is a bit weird; it's like Word split and wrapped around
--     - on 64-bit if the first 32 bits of a Word are 1s (it's negative) or all zeros, then we can truncate it to 32 smallest bits (fromIntegral does this conversion both ways)
--       - probably do this with fromIntegral (what is the safe way to check that the conversion is lossless?)
--       - see also: http://stackoverflow.com/questions/15047191/read-write-haskell-integer-in-twos-complement-representation
--     - however if we're trying to be compatible with OTHER IMPLEMENTATIONS, it's not clear we can/should do that
--     - endianness:
--       - the draft here: http://tools.ietf.org/html/draft-eastlake-fnv-08#page-5 ...
--         suggests doing arithmetic in little endian (which is nice for us on intel) so we should be able to do conversions on others
--       - can use byteSwap* functions from Data.Word (available on base 4.7 and above)
--       - we can use ByteString builder to build bytestring from various endiannesses: http://hackage.haskell.org/package/bytestring-0.10.4.1/docs/Data-ByteString-Builder.html#g:4
--          TODO + Benchmark builder with Int
--                  - super slow (200ns)
--               + test Serialized from ghc package (ghc api)
--                  - super slow (~300ns to serialize a Word)
--               x along with SmallByteString (and somehow writing Ints efficiently)
--                  - No reason to serialize things to bytestring; just hash as we go.
--               + Benchmark largeword multpilication and xor
--                  - fromIntegral is very slow; multiplication is outrageously slow
--               + How do 64-bits fare on 32-bit machines?
--               + Are there other choices of basis we can use to extend beyond 32 bit hashes?
--                 - YES! any non-zero offset basis ought to be fine, in terms of hashing.
--                 - but note: if we want cross-platform compatible, then we'll need to use their prescribed bases.
--
-- QUESTIONS:
--   - principled way to test for lossless convertable Ints / Words (maybe look at impl of fromInteger)
--   + fastest way to get access to bytes of integer types
