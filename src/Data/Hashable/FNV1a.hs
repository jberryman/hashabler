{-# LANGUAGE BangPatterns #-}
module Data.Hashable.FNV1a 
    where
    
import Data.Word
import Data.Bits

{-
-- see also the non-powers of two mapping methods outlined:
--  http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1a

For test vectors:
    http://www.isthe.com/chongo/src/fnv/test_fnv.c
-}

-- FNV CONSTANTS ----------------------------------------------------

-- The special FNV primes required for different hash output sizes:

fnvPrime32 :: Word32
fnvPrime64 :: Word64
{-# INLINE fnvPrime32 #-}
{-# INLINE fnvPrime64 #-}
fnvPrime32 = 16777619
fnvPrime64 = 1099511628211
-- fnvPrime128 = 309485009821345068724781371
-- fnvPrime256 = 374144419156711147060143317175368453031918731002211

-- The arbitrary initial seed values for different output hash sizes. These
-- values are part of the spec, but there is nothing special about them;
-- supposedly any non-zero value seed should be fine:

fnvOffsetBasis32 :: Word32
fnvOffsetBasis64 :: Word64
{-# INLINE fnvOffsetBasis32 #-}
{-# INLINE fnvOffsetBasis64 #-}
fnvOffsetBasis32 = 2166136261
fnvOffsetBasis64 = 14695981039346656037
-- fnvOffsetBasis128 = 144066263297769815596495629667062367629
-- fnvOffsetBasis256 = 100029257958052580907070968620625704837092796014241193945225284501741471925557

-- FNV HASH KERNELS -------------------------------------------------

infixl <#
(<#) :: Word32 -> Word8 -> Word32
{-# INLINE (<#) #-}
(<#) h32 b = (h32 `xor` fromIntegral b) * fnvPrime32

-- TODO 64-bit hashing


-- UNROLLED 32-BIT HASHING OF DIFFERENT VALUES: ---------------------

hash32Word32 :: Word32 -> Word32
{-# INLINE hash32Word32 #-}
hash32Word32 wd = case bytes32 wd of 
  (b0,b1,b2,b3)->
    fnvOffsetBasis32 <# b0 <# b1 <# b2 <# b3

hash32Word64 :: Word64 -> Word32
{-# INLINE hash32Word64 #-}
hash32Word64 wd = case bytes64_alt wd of
-- fnvInnerLoopTestWord64 wd = case bytes64 wd of  -- NOTE: SLOW ON 32-bit arch
  (b0,b1,b2,b3,b4,b5,b6,b7) ->
    fnvOffsetBasis32 <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7


-----------------

-- NOTE we're to hash these Word8s from left to right
bytes32 :: Word32 -> (Word8,Word8,Word8,Word8)
{-# INLINE bytes32 #-}
bytes32 wd = (shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

-- TODO benchmark on different arch's and make conditional (but expose both for
--      testing equivalence w/ quickcheck)
-- faster for 64-bit archs?
bytes64 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytes64 #-}
bytes64 wd = ( shifted 56, shifted 48, shifted 40, shifted 32
             , shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

-- faster for 32-bit archs?
bytes64_alt :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytes64_alt #-}
bytes64_alt wd = 
    let wd0 = fromIntegral $ unsafeShiftR wd 32
        wd1 = fromIntegral wd
        (b0,b1,b2,b3) = bytes32 wd0
        (b4,b5,b6,b7) = bytes32 wd1
     in (b0,b1,b2,b3,b4,b5,b6,b7)




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
--   - bytes64 is very slow on 32-bit
--   - TODO we could use SIMD vectors for wider than 32 hash bits!
--      - but need to test that this is principled.
--
-- WISHLIST:
--   - :: Word64 -> (Word32,Word32)  for 32-bit machines.
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
